--- mod-version:3

local core = require "core"
local DocView = require "core.docview"
local Doc = require "core.doc"
local command = require "core.command"
local keymap = require "core.keymap"
local common = require "core.common"
local style = require "core.style"

local coro_diff = require "libraries.coro_diff"

---Unscaled size used to align the original file names
local stop_size = 100

---@class RenamerDoc: core.doc
---@field super core.doc
local RenamerDoc = Doc:extend()

function RenamerDoc:new()
  RenamerDoc.super.new(self)
  self.initial_state = true
end

function RenamerDoc:save()
end

function RenamerDoc:raw_insert(line, col, text, ...)
  if self.initial_state then return end
  return RenamerDoc.super.raw_insert(self, line, col, text, ...)
end

function RenamerDoc:raw_remove(...)
  if self.initial_state then return end
  return RenamerDoc.super.raw_remove(self, ...)
end

---@class Renamer.file
---@field file_path string The full path of the file
---@field dir_path string The path of the directory that contains the file
---@field basename string The file name
---@field last_stop integer
---@field last_diff any? Diff cache
---@field last_file_path string? Diff cache key

---@class Renamer: core.docview
---@field super core.docview
---@field files Renamer.file[]
local Renamer = DocView:extend()

function Renamer:new()
  Renamer.super.new(self, RenamerDoc())
  self:reset()
end

function Renamer:reset()
  self.doc.initial_state = true
  self.doc:reset()
  self.last_change = self.doc:get_change_id()
  self.files = { }
  self.operations = { }
  self.errors = { }
  if self.showing_tooltip then
    core.status_view:remove_tooltip()
  end
  self.showing_tooltip = false
end

function Renamer:get_name()
  local post = self.doc:is_dirty() and "*" or ""
  local name = "Renamer"
  return name .. post
end

function Renamer:on_file_dropped(filename)
  self:add_file(filename)
  return true
end

function Renamer:add_file(filename)
  local basename = common.basename(filename)
  local path = assert(common.dirname(filename))
  for _, v in ipairs(self.files) do
    if v.file_path == filename then
      return false
    end
  end
  table.insert(self.files, {
    file_path = filename,
    dir_path = path,
    basename = basename,
    last_stop = -1,
    last_diff = nil,
    last_file_path = nil
  } --[[@as Renamer.file]])

  if self.doc.initial_state then
    self.doc.initial_state = false
    self.doc:insert(1, 1, basename)
  else
    self.doc:insert(#self.doc.lines, math.huge, "\n"..basename)
  end
  self.recalc_base_stop = true
  return true
end

function Renamer:on_mouse_moved(...)
  if self.doc.initial_state then
    self.cursor = "arrow"
  else
    Renamer.super.on_mouse_moved(self, ...)
  end
end

---Split string into a table of characters.
---@param s string
---@return string[]
local function get_utf8_chars(s)
  local result = { }
  for char in string.gmatch(s, utf8.charpattern) do
    table.insert(result, char)
  end
  return result
end


------ coro_diff definitions ----------

---@alias libraries.coro_diff.direction
---| '"+"' Added from `b`
---| '"-"' Removed from `a`
---| '"="' Same in `a` and `b`
---@alias libraries.coro_diff.solution_item {a_index: integer, a_len: integer, b_index: integer, b_len: integer, direction: libraries.coro_diff.direction, values: any[]}
---@alias libraries.coro_diff.solution libraries.coro_diff.solution_item[]

---------------------------------------


---@class Renamer.operation
---@field old_file_path string
---@field new_file_path string
---@field new_basename string
---@field diff libraries.coro_diff.solution

---@class Renamer.operation_error
---@field kind string
---@field message string

---Check if rename operations will have problems.
---@return table<integer, Renamer.operation> operations
---@return table<integer, Renamer.operation_error> errors
function Renamer:check()
  ---@type table<integer, Renamer.operation>
  local operations = { }
  ---@type table<string, integer>
  local file_path_indexes = { }
  ---@type table<integer, Renamer.operation_error>
  local errors = { }
  for i,f in ipairs(self.files) do
    ---@type Renamer.operation_error?
    local error_msg
    local new_file_path
    local new_basename = self.doc:get_text(i, 1, i, math.huge)
    if new_basename == "" then
      -- TODO: we could ask to delete files without names
      error_msg = {
        kind = "missing name",
        message = string.format("Specify a name for [%s].", self.files[i].basename)
      }
      goto continue
    end
    if new_basename:find(PATHSEP)
      or (PLATFORM == "Windows" and new_basename:find("/")) then -- On Windows always avoid /
      -- TODO: we could manage moving to new/different dirs
      error_msg = {
        kind = "invalid character",
        message = string.format("Don't use [%s].", PATHSEP)
      }
      goto continue
    end

    new_file_path = f.dir_path .. PATHSEP .. new_basename
    if f.basename ~= new_basename then
      if file_path_indexes[new_file_path] then
        error_msg = {
          kind = "conflict",
          message = string.format("This would overwrite the rename on line %d.", file_path_indexes[new_file_path])
        }
        errors[file_path_indexes[new_file_path]] = {
          kind = "conflict",
          message = string.format("This would overwrite the rename on line %d.", i)
        }
        goto continue
      elseif system.get_file_info(new_file_path) then
        -- Check if this conflicting file will be renamed in a previous operation
        local ok = false
        for _, op in pairs(operations) do
          if op and op.old_file_path == new_file_path and op.new_file_path ~= new_file_path then
            ok = true
            break
          end
        end
        if not ok then
          error_msg = {
            kind = "file already exists",
            message = string.format("This would overwrite an existing file.", i)
          }
          goto continue
        end
      end
    end
    ::continue::
    if error_msg then
    	errors[i] = error_msg
    elseif new_file_path ~= f.file_path then
      local solution
      if not f.last_diff or f.last_file_path ~= new_file_path then
        local differ_getter = coro_diff.get_diff(get_utf8_chars(f.basename), get_utf8_chars(new_basename))
        local done
        repeat
          done, solution = differ_getter(math.maxinteger)
        until done
        f.last_diff = solution
        f.last_file_path = new_file_path
      end

      file_path_indexes[new_file_path] = i
      operations[i] = {
        old_file_path = f.file_path,
        new_file_path = new_file_path,
        new_basename = new_basename,
        diff = f.last_diff
      }
    end
  end
  if #self.files > #self.doc.lines then
    errors[#self.doc.lines] = {
      kind = "missing lines",
      message = string.format("Some lines are missing.")
    }
  elseif #self.files < #self.doc.lines then
    errors[#self.doc.lines] = {
      kind = "too many lines",
      message = string.format("Too many lines.")
    }
  end
  return operations, errors
end

function Renamer:update(...)
  if not self.doc.initial_state and #self.files == 0 then
    self:reset()
  end
  if not self.doc.initial_state then
    local current_change_id = self.doc:get_change_id()
    if self.last_change ~= current_change_id then
      self.last_change = current_change_id
      self.operations, self.errors = self:check()
      if not next(self.operations) and not next(self.errors) then
        self.doc.clean_change_id = current_change_id
      end
    end

    -- Only recalc base stop offset when an entry changes the stop it belongs to
    if self.recalc_base_stop then
      self.recalc_base_stop = false
      local total_len = 0
      local max_len = 0
      for i, file in ipairs(self.files) do
        file.last_stop = -1
        local len = Renamer.super.get_col_x_offset(self, i, math.huge)
        max_len = math.max(max_len, len)
        total_len = total_len + len
      end
      self.base_stop_offset = math.min(total_len / #self.files + stop_size * SCALE, max_len)
    end

    if core.active_view == self then
      local keybind = (keymap.get_bindings("renamer:apply") or { })[1]
      core.status_view:show_tooltip(
          string.format([[Edit the file names, then run the command "%s"%s to apply the changes.]], command.prettify_name("renamer:apply"), keybind and " ["..keybind.."]" or "")
      )
      self.showing_tooltip = true
    elseif self.showing_tooltip then
      core.status_view:remove_tooltip()
      self.showing_tooltip = false
    end
  end
  Renamer.super.update(self, ...)
end

function Renamer:draw_line_gutter(line, x, y, width)
  local lh = Renamer.super.draw_line_gutter(self, line, x, y, width)
  local gw, gpad = self:get_gutter_width()
  local marker_width = 3 * SCALE
  local color
  if line > #self.files or not self.files[line] or self.errors[line] then
    color = style.error
  elseif self.doc:get_text(line, 1, line, math.huge) ~= self.files[line].basename then
    color = style.warn
  end
  if color then
    renderer.draw_rect(x + gw - (gpad / 2 + marker_width) / 2, y, marker_width, lh, color)
  end
  return lh
end


function Renamer:draw_line_text(line, x, y)
  local lh = self:get_line_height()
  if self.errors[line] or not self.operations[line] then
    Renamer.super.draw_line_text(self, line, x, y)
  end

  local x_off = Renamer.super.get_col_x_offset(self, line, math.huge)
  if self.errors[line] then
    renderer.draw_text(
      style.code_font,
      self.errors[line].message,
      x + x_off,
      y + self:get_line_text_y_offset(),
      style.warn
    )
  elseif self.operations[line] then
    local op = self.operations[line]

    local x2 = x + style.padding.x
    local stop = (x_off // (stop_size * SCALE) + 1)
    local avg_stop = (self.base_stop_offset // (stop_size * SCALE) + 1)
    x2 = x2 + math.max(stop, avg_stop) * (stop_size * SCALE)
    if stop ~= self.files[line].last_stop then
      if self.files[line].last_stop >= 0 then
      	self.recalc_base_stop = true
      end
      self.files[line].last_stop = stop
    end

    for _, d in ipairs(op.diff) do
      local char = table.concat(d.values)
      local color
      if d.direction == "-" then
        color = style.warn
        char = char:gsub(" ", "·")
      elseif d.direction == "+" then
        color = style.good
        char = char:gsub(" ", "·")
      end
      if d.direction ~= "-" then
        x = renderer.draw_text(style.code_font, char, x, y + self:get_line_text_y_offset(), color or style.syntax["normal"])
      end
      if d.direction ~= "+" then
        x2 = renderer.draw_text(style.code_font, char, x2, y + self:get_line_text_y_offset(), color or style.text)
      end
    end
  end
  return lh
end

function Renamer:draw(...)
  if self.doc.initial_state then
    local drop_text = { "Drop files", "to rename", "here" }
    local full_text = table.concat(drop_text, " ")
    local too_big = style.big_font:get_width(full_text) >= self.size.x
    local h = style.big_font:get_height()
    self:draw_background(style.background)
    if too_big then
      for i, part in ipairs(drop_text) do
        local offset_y = (i - ((#drop_text + 1) / 2)) * h
        common.draw_text(style.big_font, style.accent, part, "center", self.position.x, self.position.y + offset_y, self.size.x, self.size.y)
      end
    else
      common.draw_text(style.big_font, style.accent, full_text, "center", self.position.x, self.position.y, self.size.x, self.size.y)
    end
  else
    Renamer.super.draw(self, ...)
  end
end

command.add(nil, {
  ["renamer:open-renamer"] = function()
    local node = core.root_view:get_active_node_default()
    local view = Renamer()
    node:add_view(view)
  end
})

---@param dry_run boolean?
function Renamer:apply(dry_run)
  if #self.doc.lines ~= #self.files then
    return core.error("Unable to apply renames. Mismatching number of lines.")
  end
  local operations, errors = { }, { }
  operations, errors = self:check()
  for index, error in pairs(errors) do
    return core.error("Unable to apply renames. Issue on line %d: %s", index, error.message)
  end

  self.last_change = -1
  local renamed = 0
  for i,f in ipairs(self.files) do
    local op = operations[i]
    if op then
      local src = op.old_file_path
      local dst = op.new_file_path
      if src ~= dst then
        core.log_quiet("Renaming [%s] to [%s].", src, dst)
        local ok, err
        if dry_run then
          ok, err = true, nil
        else
          ok, err = os.rename(src, dst)
        end
        if not ok then
          core.error("Failed to rename [%s] to [%s]. Message: %s", src, dst, err)
          return
        end
        renamed = renamed + 1
        f.file_path = dst
        f.basename = op.new_basename
      end
    end
  end
  core.log("Renamed %d entries.", renamed)
end

command.add(
  function()
    return core.active_view:extends(Renamer) and #core.active_view.files > 0, core.active_view
  end,
  {
    ["renamer:apply"] = function(renamer)
      ---@cast renamer Renamer
      renamer:apply()
    end,
    ["renamer:apply-dry-run"] = function(renamer)
      ---@cast renamer Renamer
      renamer:apply(true)
    end,
    ["renamer:reset"] = function(renamer)
      ---@cast renamer Renamer
      renamer:reset()
    end,
    ["renamer:restore-selected"] = function(renamer)
      ---@cast renamer Renamer
      for _, l1, _, l2, _ in renamer.doc:get_selections(true, true) do
        for i=l2,l1,-1 do
          -- Insert first to avoid issues
          renamer.doc:insert(i, 1, renamer.files[i].basename)
          renamer.doc:remove(i, #renamer.files[i].basename + 1, i, math.huge)
        end
      end
    end,
    ["renamer:restore-everything"] = function(renamer)
      ---@cast renamer Renamer
      local old_files = renamer.files
      renamer:reset()
      for _,file in ipairs(old_files) do
        renamer:add_file(file.file_path)
      end
    end,
    ["renamer:remove"] = function(renamer)
      ---@cast renamer Renamer
      for _, l1, _, l2, _ in renamer.doc:get_selections(true, true) do
        renamer.doc:remove(l1 - 1, math.huge, l2, math.huge)
        for i=l2,l1,-1 do
          table.remove(renamer.files, i)
        end
      end
    end
  }
)

keymap.add({
  ["ctrl+s"] = "renamer:apply"
})

local enumerate_regex = assert(regex.compile("^(\\d+)(?:,(\\d+))?(?:,(\\d+))?$"))
command.add(DocView, {
  ["renamer:enumerate"] = function(dv)
    core.command_view:enter("Specify [size[,initial index[,step]]] (e.g. 3,5 -> 005, 006, 007, ...)", {
      submit = function(text, _)
        local size, initial, step = regex.match(enumerate_regex, text)
        size = size and tonumber(size) or 1
        initial = initial and tonumber(initial) or 1
        step = step and tonumber(step) or 1
        for idx in dv.doc:get_selections() do
          local value = string.format("%0"..size.."d", initial)
          dv.doc:text_input(value, idx)
          initial = initial + step
        end
      end,
      validate = function(text, _)
        return regex.match(enumerate_regex, text) or text == ""
      end
    })
  end
})

-- Workaround needed because data/core/commands/findreplace.lua compares to DocView strictly
function Renamer:is(T)
  if T == DocView then return true end
  return self.super.is(self, T)
end

return Renamer

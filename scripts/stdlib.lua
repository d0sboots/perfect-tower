STDLIB = [===[
#pos.relative(x_pos, y_pos, x_anchor, y_anchor)={{lua(
  local x_pos = [==[{x_pos}]==]
  local y_pos = [==[{y_pos}]==]
  local x_anchor = [==[{x_anchor}]==]
  local y_anchor = [==[{y_anchor}]==]

  local function eq_part(term1, pos, anchor, mult)
    local pos_num, anchor_num = tonumber(pos), tonumber(anchor)

    local diff = pos_num and anchor_num and pos_num - anchor_num or nil
    local first_part = "0."
    if diff ~= 0.0 and pos ~= anchor then
      first_part = term1
      if diff ~= 1.0 then
        first_part = first_part .. " * ((" .. pos .. ") - (" .. anchor .. "))"
      end
    end

    local last_part = ""
    if anchor_num ~= 0.0 then
      if first_part == "0." then
        first_part = ""
      else
        last_part = " + "
      end
      last_part = last_part .. mult
      if anchor_num ~= 1.0 then
        last_part = last_part .. " * (" .. anchor .. ")"
      end
    end
    return first_part .. last_part
  end

  return "vec(" ..
    eq_part("min(if(option.get('ui.dynamicScaling'), 1024., 1./0.), min(width.d(), (16./9.) * height.d())) * if(option.get('ui.dynamicScaling') && min(width.d(), (16./9.) * height.d()) > 1024., 1., ui.size())",
            x_pos, x_anchor, "width.d()") ..
    ", " ..
    eq_part("min(if(option.get('ui.dynamicScaling'), 576., 1./0.), min(height.d(), (9./16.) * width.d())) * if(option.get('ui.dynamicScaling') && min(height.d(), (9./16.) * width.d()) > 576., 1., ui.size())",
            y_pos, y_anchor, "height.d()") ..
    ")"
)}}

#click.relative(x_pos, y_pos, x_anchor, y_anchor) click({pos.relative({x_pos},{y_pos},{x_anchor},{y_anchor})})
]===]

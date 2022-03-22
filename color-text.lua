Span = function(el)
  color = el.attributes['color']
  strike = el.attributes['strike']
  hide = el.attributes['hide']
  highlight = el.attributes['highlight']
  -- if no color attribute, return unchange
  if color ~= nil then
  
    -- transform to <span style="color: red;"></span>
    if FORMAT:match 'html' then
      -- remove color attributes
      el.attributes['color'] = nil
      -- use style attribute instead
      el.attributes['style'] = 'color: ' .. color .. ';'
      -- return full span element
      return el
    elseif FORMAT:match 'latex' then
      print("formatting color for latex")
      -- remove color attributes
      el.attributes['color'] = nil
      -- encapsulate in latex code
      table.insert(
        el.content, 1,
        pandoc.RawInline('latex', '\\textcolor{'..color..'}{')
      )
      table.insert(
        el.content,
        pandoc.RawInline('latex', '}')
      )
      -- returns only span content
      return el.content
    else
      -- for other format return unchanged
      return el
    end

  print("formatting strikethroughs")
  -- if no strike attribute, return unchanged
  elseif strike ~= nil then
    
    -- transform to proper html format
    if FORMAT:match 'html' then
      -- remove strike attributes
      el.attributes['strike'] = nil
      -- use 
      return el
    elseif FORMAT:match 'latex' then
      print("formatting strikethroughs for latex")
      -- remove srtike attributes
      el.attributes['strike'] = nil
      if strike == 'show' then
        print("showing strikethroughs")
        -- encapsulate in latex code
        table.insert(
          el.content, 1,
          pandoc.RawInline('latex', '\\sout{')
        )
        table.insert(
          el.content,
          pandoc.RawInline('latex', '}')
        )
        -- returns only span content
        return el.content
      elseif strike == 'hide' then
        el.content = {nil}
        return el.content
      end
      
    else
      -- for other format return unchanged
      return el
    end
    
      print("formatting hidden text")
  -- if no strike attribute, return unchanged
  elseif hide ~= nil then
    
    -- transform to proper html format
    if FORMAT:match 'html' then
      -- remove strike attributes
      el.attributes['hide'] = nil
      -- use 
      return el
    elseif FORMAT:match 'latex' then
      print("formatting strikethroughs for latex")
      -- remove srtike attributes
      el.attributes['hide'] = nil
      if hide == 'TRUE' then
        el.content = {nil}
        return el.content
      end
      
    else
      -- for other format return unchanged
      return el
    end
  elseif highlight ~= nil then
    
    -- transform to proper html format
    if FORMAT:match 'html' then
      -- remove strike attributes
      el.attributes['highlight'] = nil
      -- use 
      return el
    elseif FORMAT:match 'latex' then
      print("formatting highlights for latex")
      -- remove srtike attributes
      el.attributes['highlight'] = nil
      if highlight == 'TRUE' then
        table.insert(
        el.content, 1,
        pandoc.RawInline('latex', '\\hl{')
      )
      table.insert(
        el.content,
        pandoc.RawInline('latex', '}')
      )
        return el.content
      end
      
    else
      -- for other format return unchanged
      return el
    end
    
  else return el end
end
function vterm_prompt_end;
  vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
end

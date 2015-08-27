{module, math}.  %% version = 0

{exports, [{append,1},{append,2},{inc,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 11}.


{function, inc, 1, 2}.
  {label,1}.
    {line,[{location,"math.erl",4}]}.
    {func_info,{atom,math},{atom,inc},1}.
  {label,2}.
    {line,[{location,"math.erl",5}]}.
    {gc_bif,'+',{f,0},1,[{x,0},{integer,1}],{x,0}}.
    return.


{function, append, 2, 4}.
  {label,3}.
    {line,[{location,"math.erl",7}]}.
    {func_info,{atom,math},{atom,append},2}.
  {label,4}.
    {line,[{location,"math.erl",8}]}.
    {call_ext_only,2,{extfunc,lists,append,2}}.


{function, append, 1, 6}.
  {label,5}.
    {line,[{location,"math.erl",10}]}.
    {func_info,{atom,math},{atom,append},1}.
  {label,6}.
    {move,{literal,[1,2,3]},{x,1}}.
    {line,[{location,"math.erl",11}]}.
    {call_ext_only,2,{extfunc,lists,append,2}}.


{function, module_info, 0, 8}.
  {label,7}.
    {line,[]}.
    {func_info,{atom,math},{atom,module_info},0}.
  {label,8}.
    {move,{atom,math},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 10}.
  {label,9}.
    {line,[]}.
    {func_info,{atom,math},{atom,module_info},1}.
  {label,10}.
    {move,{x,0},{x,1}}.
    {move,{atom,math},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.

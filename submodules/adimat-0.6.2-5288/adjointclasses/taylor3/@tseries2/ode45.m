% function [t, y_obj] = ode45(func, ts, y0)
%
% Copyright (c) 2018 Johannes Willkomm <johannes@johannes-willkomm.de>
%
function [t, y_obj] = ode45(func, ts, y0)
  [t, y_obj] = ode_generic(@ode45, func, ts, y0);

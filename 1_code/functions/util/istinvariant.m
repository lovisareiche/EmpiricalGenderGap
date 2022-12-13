function [ isti, diff ] = istinvariant( id,  X, tol )
%ISTINVARIANT Internal Function
%   Internal Function
%
%   Copyright 2013-2015 Inmaculada C. Álvarez, Javier Barbero, José L. Zofío
%   http://www.paneldatatoolbox.com
%
%   Version: 2.0
%   LAST UPDATE: 17, June, 2015
%
    % Default tolerance
    if nargin < 4
        tol = 1e-2;
    end

    % make table if it isn't already
    if istable(X)
        X=table2array(X);
    end

    % Compute group means and replicate for all observations
    [~,~,ic] = unique(id);

    for i = 1:width(X)
        a = accumarray(ic,X(:,i),[],@mean);
        Xbar(:,i) = a(ic);
    end
    
    % Substract means
    diff = X - Xbar;
    
    % Check if all are zero
    isti = sum(abs(diff))/height(X) < tol;

end


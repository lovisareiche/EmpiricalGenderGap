function est = tipanel( id, time, y, X, W, varargin)

%  Panel data estimation using the Hausman Taylor (1981) estimator
%   Computes panel data estimation for endogenous and exogenous time
%   invariant and time varying variables.
%
%   est = PANEL(id, time, y, X, method) computes a panel data estimation of
%   y into X1, X2, W1, W2, where id and time are the individual
%   and time identifiers. The function returns an estimation ouput structure,
%   estout.
%   est = PANEL(id, time, y, X, method, Name,Value) computes panel data 
%   estimation with additional properties using one or more Name,Value pair
%   arguments.

% INPUT
% id: individual specific id
% time: time variable (ie year)
% y: dependent variable
% X: time varying vars
% W: time invariant vars

%   Additional properties:
%   - 'vartype': compute heteroskedasticity 'robust' or 'cluster' variance 
%   matrix estiamtion. Default 'homo' for homoskedasticity.
%   - 'clusterid': specify the cluster variable. Only if 'vartype' is set
%   to 'cluster'. Default equal to id.
%   - 'dfcorrection': 0 to supress degrees-of-freedom correction of the
%   variance. Default 1.


    % Create output structure
    est = estout();
    
    % Check input
    if nargin < 3
        error('Dependent variable not specified');
    end
    if nargin < 5
        error('Independent variables not specified');
    end
    if size(y,2) ~= 1
        error('Y must be a column vector of the dependant variable');
    end
    if size(y,1) ~= size(X,1)
        error('Number of rows in Y must be equal to number of rows in X');
    end
    if size(X,1) ~= size(W,1)
        error('Number of rows in X must be equal to number of rows in W');
    end
    if size(id,1) ~= size(y,1)
        error('Number of rows in id must be equal to number of rows in Y');
    end
    if size(time,1) ~= size(y,1)
        error('Number of rows in time must be equal to number of rows in Y');
    end

    % Parse Additional options
    p = inputParser;
    if verLessThan('matlab', '8.2')
        addPar = @(v1,v2,v3,v4) addParamValue(v1,v2,v3,v4);
    else
        addPar = @(v1,v2,v3,v4) addParameter(v1,v2,v3,v4);
    end
    addPar(p,'vartype','homo',...
                 @(x) any(validatestring(x,{'homo','robust','cluster'})));
    addPar(p,'dfcorrection',1,@(x) isnumeric(x));
    addPar(p,'clusterid',id,@(x) length(x) == length(y))
    p.parse(varargin{:})
    options = p.Results;
    
    
    % Extract table names and convert data to array
    id = extracttable(id);
    time = extracttable(time);
    [y, ynames] = extracttable(y);
    [X, xnames] = extracttable(X);
    [W, wnames] = extracttable(W);
    options.clusterid = extracttable(options.clusterid);
    
    % Error if NaN's in input data
    if any(isnan(id)) ||any(isnan(time)) || any(isnan(y)) || any(any(isnan(X))) || any(any(isnan(W)))
        error('NaN values not allowed in input data. Remove all rows with NaN''s before using this function.');
    end
    
    % Automatically change 'robust' to 'cluster'
    if strcmp(options.vartype,'robust') 
        options.vartype = 'cluster';        
    end
    
    % Check if robust
    if strcmp(options.vartype,'cluster')
        robust = 1;
    else
        robust = 0;
    end
    
    % Get number of observations
    N = size(y,1); 
    
    % Get number of
    k = size(X,2); % exogenous time varying
    l = size(W,2); % exogenous time invariant
    
    % Get balanced and T variables
    [ isBalanced, idx, n, T, Tid, Tmean, Thmean] = isbalancedpanel( id, time );
    
    % Sort variables
    id = id(idx);
    time = time(idx);
    y = y(idx,:);
    X = X(idx,:);
    W = W(idx,:);
    
    % Store original data (sorted)
    est.id = id;
    est.uid = unique(id);
    est.time = time;
    est.idx = idx;
    est.y = y;
    est.X = X;
    est.W = W;
 
%% Step 1: Regress the model by FE by using differences from the 
    % “temporal” mean, ie using fixed effects estimator;
    
    [est1] = panel( id, time, y, X, 'fe' );
    
%% Step 2: Estimate gamma as OLS in Res = W*gamma + error
    
    est2 = panel(id, time, est1.res, W, 'po','vartype','cluster','clusterid',id);
    
%% Other
    % Compute estimates
    coef_tv = est1.coef;
    coef_ti = est2.coef;
    
    % Fitted values
    yhat = X*coef_tv + [W ones(size(y))]*coef_ti;
    
    % Residuals
    res = y - yhat;
    
    % Inverse of X'X
    Xtr=[X W ones(size(y))];
    invXtrXtr = ((Xtr'*Xtr)\eye(k+l+1));
    
    % Residual variance
    resdf = N-k-l;
    resvar = (res'*res) ./ resdf;
    
    % Covariance matrix
    switch options.vartype
        case 'homo'
            varcoef = resvar * invXtrXtr;
        case {'robust','cluster'}
            % White-robust standard errors adjusted for cluster
            % Get unique ids
            id_clus = options.clusterid;
            uid_clus = unique(id_clus);
            n_clus = length(uid_clus);
            
            % Compute total sum
            total_sum = 0;
            for i=1:n_clus
                total_sum = total_sum + Xtr(id_clus==uid_clus(i),:)'*res(id_clus==uid_clus(i))*res(id_clus==uid_clus(i))'*Xtr(id_clus==uid_clus(i),:);
            end
            varcoef = invXtrXtr * total_sum * invXtrXtr; 
              
            % Degrees of freedom correction
            if options.dfcorrection
                    varcoef = (n_clus/(n_clus-1)) * ((N-1)/(N-k-l)) .* varcoef;
            end
            
            % Change resdf to the correct value with cluster
            resdf = n_clus - 1;
    end

    % Standard errors
    stderr = sqrt(diag(varcoef));
    
    % Goodness of fit
    %    M0 = eye(N) - 1/N * ones(N);
        if  robust
            adjr2_correction = (N - 1) ./ ((N-n)-k-l); 
        else
            adjr2_correction = (N - 1) ./ (resdf);
        end
    
    RSS = res' * res;
    TSS = y' * y;
    ESS = TSS - RSS;
    %r2 = 1 - (res' * M0 * res) ./ (ytr' * M0 * ytr);
    r2 = 1 - RSS ./ sum((y - mean(y)).^2);
    adjr2 = 1 - adjr2_correction .* (1 - r2);

    % Save estimation
    est.options = options;
    est.method = 'TIP';
    est.n = n;
    est.T = T;
    est.N = N;
    est.k = k + l +1;
    est.isPanel = 1;
    est.isBalanced = isBalanced;
    est.isMultiEq = 0;
    est.isLinear = 1;
    est.isRobust = robust;

    est.coef = [coef_tv; coef_ti];
    est.varcoef = varcoef;
    est.stderr = stderr;
    est.yhat = yhat;
    est.res = res;
    est.resvar = resvar;
    est.resdf = resdf;
    
    est.Tid = Tid;
    est.Tmean = Tmean;
    est.Thmean = Thmean;
    
    est.RSS = RSS;
    est.ESS = ESS;
    est.TSS = TSS;
    est.r2 = r2;
    est.adjr2 = adjr2;    
    est.isAsymptotic = 0;
    est.vartype = options.vartype;
            
    % Set default var names
    est.ynames = ynames;
    est.xnames = [xnames wnames 'const'];
    est = defaultVarNames(est);
    

    
end
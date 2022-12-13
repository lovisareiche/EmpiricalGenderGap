function est = hmtpanel( id, time, y, X1, X2, W1, W2 )

% HMTPANEL Panel data estimation using the Hausman Taylor (1981) estimator
%   Computes panel data estimation for endogenous and exogenous time
%   invariant and time varying variables.
%
%   est = PANEL(id, time, y, X, method) computes a panel data estimation of
%   y into X1, X2, W1, W2, where id and time are the individual
%   and time identifiers. The function returns an estimation ouput 
%   structure, estout.
%   est = PANEL(id, time, y, X, method, Name,Value) computes panel data 
%   estimation with additional properties using one or more Name,Value pair
%   arguments.

% Compatible with Toolbox by Alvarez et al

% INPUT
% id: individual specific id
% time: time variable (ie year)
% y: dependent variable
% X1: exogenous time varying vars
% X2: endogenous time varying vars
% W1: exogenous time invariant vars
% W2: endogenous time invariant vars

    % Create output structure
    est = estout();
    
    % Check input
    if nargin < 3
        error('Dependent variable not specified');
    end
    if nargin < 7
        error('Independent variables not specified');
    end
    if size(y,2) ~= 1
        error('Y must be a column vector of the dependant variable');
    end
    if size(y,1) ~= size(X1,1)
        error('Number of rows in Y must be equal to number of rows in X');
    end
    if size(X1,1) ~= size(X2,1)
        error('Number of rows in X1 must be equal to number of rows in X2');
    end
    if size(X2,1) ~= size(W1,1)
        error('Number of rows in X must be equal to number of rows in W');
    end
    if size(W1,1) ~= size(W2,1)
        error('Number of rows in W1 must be equal to number of rows in W2');
    end
    if size(id,1) ~= size(y,1)
        error('Number of rows in id must be equal to number of rows in Y');
    end
    if size(time,1) ~= size(y,1)
        error('Number of rows in time must be equal to number of rows in Y');
    end

    % Extract table names and convert data to array
    id = extracttable(id);
    time = extracttable(time);
    [y,~] = extracttable(y);
    [X1, x1names] = extracttable(X1);
    [X2, x2names] = extracttable(X2);
    [W1, w1names] = extracttable(W1);
    [W2, w2names] = extracttable(W2);
    
    % Error if NaN's in input data
    if any(isnan(id)) ||any(isnan(time)) || any(isnan(y)) || any(any(isnan(X1))) || any(any(isnan(X2))) || any(any(isnan(W1))) || any(any(isnan(W2)))
        error('NaN values not allowed in input data. Remove all rows with NaN''s before using this function.');
    end
    
    % Get number of observations
    N = size(y,1); 
    
    % Get number of
    k1 = size(X1,2); % exogenous time varying
    k2 = size(X2,2); % endogenous time varying
    l1 = size(W1,2); % exogenous time invariant
    l2 = size(W2,2); % endogenous time invariant
    
    % Get balanced and T variables
    [ isBalanced, idx, n, T, Tid, Tmean, Thmean] = isbalancedpanel( id, time );
    
    % Sort variables
    id = id(idx);
    time = time(idx);
    y = y(idx,:);
    X1 = X1(idx,:);
    X2 = X2(idx,:);
    W1 = W1(idx,:);
    W2 = W2(idx,:);
    
    % Store original data (sorted)
    est.id = id;
    est.uid = unique(id);
    est.time = time;
    est.idx = idx;
    est.y = y;
    est.X1 = X1;
    est.X2 = X2;
    est.W1 = W1;
    est.W2 = W2;
    
% Step 1: Regress the model by OLS by using differences from the 
% “temporal” mean, ie using fixed effects estimator;
    
    X= [X1, X2];
    [ est1 ] = panel( id, time, y, X, 'fe' );
    coef = est1.coef;
    
% Step 2: (a) From Step 1, use the residual to compute the 
% “intra-group”  temporal mean of the residuals;
% (b) and stack them into vector eta
    
    ebar = zeros(est1.n,1);
    etabar = zeros(est1.N,1);
    
    for i = 1:est1.n
        
        ebar(i) = sum(est1.res(est1.id == est1.uid(i)))/est1.Tid(i); % computes average residual
        etabar(est1.id == est1.uid(i)) = ebar(i); % assigns to location in etabar
             
    end
    
% Step 3: perform a 2SLS regression
% (a) estimate endogenous time invariant vars W2 using pooled OLS and all
% exogenous variables
% (b) use fitted values in a regression of etabar (mean residuals from regressing y on all time varying) on W1 and W2hat (now
% exogenous)

    W2hat = zeros(N,l2);
    
    for i = 1:l2
        str = "var" + i;
        est2.(str) = panel(id, time, W2(:,i), [X1 W1], 'po','vartype','cluster','clusterid',id);
        W2hat(:,i) = est2.(str).yhat;
    end
    
    est3 = panel(id, time, etabar, [W1 W2hat], 'po','vartype','cluster','clusterid',id);
    
% Step 4:  Estimate  var u from the regression in step 1 and use the
% estimate of var u  from the 2SLS regression to obtain theta
   
    
    sigma2u = est3.resvar - est1.resvar/T;
    
    theta = 1-sqrt(est1.resvar/(est1.resvar + sigma2u)/T);
    
% Step 5: 2SLS

    
    Vstar = [X1, X2, W1, W2, ones(height(X1),1)] - theta*[X1, X2, W1, W2, ones(height(X1),1)];
    ystar = y - theta*y;
    
    Z = [(X1 - groupmeans(id,X1,'replicate',1)),(X2 - groupmeans(id,X2,'replicate',1))];
    
    % Regress Vstar on Z and generate Vstarhat
    for i=1:size(Vstar,2)-1
        est4{i} = panel(id, time, Vstar(:,i), Z, 'po','vartype','cluster','clusterid',id);
        Vstarhat(:,i) = est4{i}.yhat;
    end
    
    % Regress ystar on Vstarhat to get coefficients alpha/betahat
    est5 = panel(id, time, ystar, Vstarhat, 'po','vartype','cluster','clusterid',id);
    
% Output Structure

    est.method = 'HMT';
    est.hasConstant = est5.hasConstant;
    est.isLinear = est5.isLinear;
    est.isInstrumental = 0;
    est.isPanel = est5.isPanel;
    est.isRobust = est5.isRobust;
    est.isAsymptomtic = est5.isAsymptotic;
    est.isSpatial = est5.isSpatial;
    est.isMultiEq = est5.isMultiEq;
    est.ynames = {'deptvar'};
    est.xnames = [x1names x2names w1names w2names {'CONST'}];
    est.znames = NaN;
    est.X = [X1 X2];
    est.Z = Z;
    est.W = [W1 W2];
    est.Xhat = Vstarhat;
    est.n = n;
    est.T = T;
    est.N = N;
    est.k = k1+k2+l1+l2+1;
    est.isBalanced = isBalanced;
    est.coef = est5.coef;
    est.yhattr = est5.yhat;
    %est.yhat = [est.X est.W ones(height(X1),1)]*est.coef;
    est.resdf = N - est.k;
    est.res = ystar - est.yhattr;
    est.resvar = (est.res'*est.res) ./ est.resdf;
    est.varcoef = est.resvar * (((Vstar'*Vstar)\eye(est.k)));
    est.stderr = sqrt(diag(est.varcoef));
    est.RSS = sum((est.res).^2);
    est.TSS = sum((ystar - mean(ystar)).^2);
    est.ESS = est.TSS - est.RSS;
    est.r2 = 1 - est.RSS ./ est.TSS;
    est.adjr2 = 1 - ((N - 1) ./ (est.resdf)) .* (1 - est.r2);
    est.Tid = Tid;
    est.Tmean = Tmean;
    est.Thmean = Thmean;
    est.vartype = 'robust';
    
    
    
    
end
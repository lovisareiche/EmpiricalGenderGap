%------------
%% Introduction
% ------------

% This file runs the first regression, the OLS regression

clear
NAME = 'code07_iv.m';
PROJECT = 'EmpiricalGenderGap';
PROJECT_DIR = 'D:\Lovisa\Studium\Oxford\Department of Economics\DPhil';

% ------------
%% Preamble
% ------------

% ---------------------
% Set working directory
% ---------------------
% The code below will traverse the path upwards until it finds the root folder of the project.

cd(fullfile(PROJECT_DIR, PROJECT))
addpath(genpath(fullfile(PROJECT_DIR, PROJECT,'empirical','1_code','functions')))


% --------
% Settings
% --------
% Any settings go here

t = 'base';

% -- Load data from another pipeline folder 03

load(fullfile('empirical', '2_pipeline', 'code03_compilepanel.m', 'out',t, 'T_cleaned.mat'),'T','y','wave','id','T_fin','y_fin','wave_fin','id_fin','w')

% ----------------------------------
% Set  up pipeline folder if missing
% ----------------------------------
% The code below will automatically create a pipeline folder for this code file if it does not exist.

if exist(fullfile('empirical', '2_pipeline'))
  pipeline = fullfile('empirical', '2_pipeline', NAME);
else
  pipeline = fullfile('2_pipeline', NAME);
end

if ~exist(pipeline,'dir')
  mkdir(pipeline)
  for folder = {'out', 'store', 'tmp'}
    mkdir(char(fullfile(pipeline, folder)))
  end
  clear folder
end

if ~exist(fullfile(pipeline,'out',t),'dir')
    mkdir(char(fullfile(pipeline, 'out',t)))
end


% ---------
%% Main code
% ---------

% --- check which variables are correlated with financial literacy

% goal: create dummy variable for significant correlation
fls_corr = zeros(1,width(T));
flt_corr = zeros(1,width(T));

for i=1:width(T_fin)
    [~,p] = corr(T_fin.(i),table2array(T_fin(:,strcmp(T_fin.Properties.VariableNames,'fin_lit_subj'))));
    % reject null of no correlation if p is larger or equal to 0.05
    if p>=0.05
        fls_corr(i) = 1;
    end
    [~,p] = corr(T_fin.(i),table2array(T_fin(:,strcmp(T_fin.Properties.VariableNames,'fin_lit_test'))));
    % reject null of no correlation if p is larger or equal to 0.05
    if p>=0.05
        flt_corr(i) = 1;
    end
end

% remove own correlations
fls_corr = fls_corr(and(~strcmp(T_fin.Properties.VariableNames,'fin_lit_subj'),~strcmp(T_fin.Properties.VariableNames,'fin_lit_test')));
flt_corr = flt_corr(and(~strcmp(T_fin.Properties.VariableNames,'fin_lit_subj'),~strcmp(T_fin.Properties.VariableNames,'fin_lit_test')));

% is endogenous if correlated with one of the two
isexo = T.Properties.VariableNames(fls_corr + flt_corr == 0);
isendo = T.Properties.VariableNames(fls_corr + flt_corr > 0);

% --- check which variables are correlated with inflation expectations
% (i.e. only instruments)

% goal: create dummy variable for significant correlation
y_corr = zeros(1,width(T));

for i=1:width(T)
    [~,p] = corr(T.(i),y);
    % reject null of no correlation if p is larger or equal to 0.05
    if p>=0.05
        y_corr(i) = 1;
    end
end

% is good instrument if uncorrelated
isi = T.Properties.VariableNames(y_corr == 0);

% --- group variables

% X1: exogenous and explaining y
X1 = table;
for i = 1:width(T)
    ii = char(T.Properties.VariableNames(i));
    if sum(strcmp(isexo,ii)) == 1 &&  sum(strcmp(isi,ii)) == 0
        X1.(ii) = T.(ii);
    end
end 
% X2: endogenous and explaining y
X2 = table;
for i = 1:width(T)
    ii = char(T.Properties.VariableNames(i));
    if sum(strcmp(isendo,ii)) == 1 &&  sum(strcmp(isi,ii)) == 0
        X2.(ii) = T.(ii);
    end
end
% IV: instruments
IV = table;
for i = 1:width(T)
    ii = char(T.Properties.VariableNames(i));
    if sum(strcmp(isexo,ii)) == 1 &&  sum(strcmp(isi,ii)) == 1
        IV.(ii) = T.(ii);
    end
end


% --- first step: estimate endogenous variables using instruments and use
% their predicted variables

X2hat = table;
for i=1:width(X2)
    ii = char(X2.Properties.VariableNames(i));
    est = panel(id, wave, table2array(X2(:,i)), [X1 IV], 'po','vartype','cluster','clusterid',id); % estimate endogenous variables
    X2hat.(ii) = est.yhat; % use their predictor
end

% --- second step: use the predicted values in the actual regression
% instead of the endogenous variables

estIV = panel(id, wave, y, [X1 X2hat], 'po','vartype','cluster','clusterid',id); % estimate endogenous variables
printIV = estdisp(estIV);


%% -- Save data to pipeline folder -- 

save(fullfile(pipeline, 'out',t, 'T.mat'),'T','estIV','printIV','y','wave','id','X1','X2','IV','X2hat','w',"NAME","pipeline",'PROJECT','PROJECT_DIR')



% ----------
% Leftovers
% ----------

%% Here you leave any code snippets or temporary code that you don't need but don't want to delete just yet

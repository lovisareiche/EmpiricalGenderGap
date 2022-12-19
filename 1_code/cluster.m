%------------
%% Introduction
% ------------

% This file compiles the final panel in one table. It also removes
% variables which have a lot of missing values.

clear
NAME = 'cluster.m';
PROJECT = 'EmpiricalGenderGap';
PROJECT_DIR = 'D:\Lovisa\Studium\Oxford\Department of Economics\DPhil';

% ------------
%% Preamble
% ------------

% -- Load data from another pipeline folder --

load(fullfile('empirical', '2_pipeline', 'code03_compilepanel.m', 'out','base', 'T_cleaned.mat'),'T','y','wave','id','w')

% --------
% Settings
% --------
% Any settings go here

vars = {'shop_groceries','shop_major','prep_meals','decide_finance'};

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


% ---------
%% Main code
% ---------


% Step 1: Set the variables to be clustered
W = table2array(T(:,vars));


%% Step 2: Clustering
%{
% Set the optimal number of clusters by maximising the average silhuette
avs=zeros(10,1);
for k=1:10
    idx_certainty = kmedoids(W,k,'Algorithm','pam','Distance',@gower_distfun);
    s=silhouette(W,idx_certainty,@gower_distfun);
    avs(k)=mean(s);
end
cnt=1:10;
plot(cnt,avs)
clear s

k=cnt(avs==max(avs));
%}
k=2;

% Run Clustering

% [idx,~,~,~,~,~] = kmedoids(W,k,'Algorithm','pam','Distance',@gower_distfun);
% idx=double(idx==1);

idx = kmeans(W,k,'Distance','cityblock');
idx=double(idx==1);

writetable(table(idx,'VariableNames',{'hhcluster'}),fullfile(pipeline, 'out','hhcluster.csv'))


%% Step 3. Look at cluster

% load in case un only from here
idx = readmatrix(fullfile(pipeline, 'out','hhcluster.csv'));

M = [mean(T.shop_groceries(idx==0)) mean(T.shop_groceries(idx==1));...
    mean(T.shop_major(idx==0)) mean(T.shop_major(idx==1));...
    mean(T.prep_meals(idx==0)) mean(T.prep_meals(idx==1));...
    mean(T.decide_finance(idx==0)) mean(T.decide_finance(idx==1))];
W = array2table(M,'VariableNames',{'hhcluster0','hhcluster1'}, 'RowNames',{'shop_groceries','shop_major','prep_meals','decide_finance'});
writetable(W,fullfile(pipeline, 'out','hhclustercomp.csv'))


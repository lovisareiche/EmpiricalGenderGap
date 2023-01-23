%------------
%% Introduction
% ------------

% This file compiles the final panel in one table. It also removes
% variables which have a lot of missing values.

clear
NAME = 'code03_compilepanel.m';
PROJECT = 'EmpiricalGenderGap';
PROJECT_DIR = 'D:\Lovisa\Studium\Oxford\Department of Economics\DPhil';

% ------------
%% Preamble
% ------------

% -- Load data from another pipeline folder --

load(fullfile('empirical', '2_pipeline', 'code02_prepvars.m', 'out', 'W_pesshop.mat'),'W','w')

% --------
% Settings
% --------
% Any settings go here

% which waves
w = 2:33;

% type
t = 'female_only';
% types are: int no_pessimist no_quali no_shopintent no_feedback no_edu
% no_inc no_hhroles no_uncertainty no_employ no_geo

% select vars to be included in the panel
vars = {'pessimist','q_unemployment','q_rent','q_lending','q_interest',...
    'q_inflation','q_property','q_growth','q_fuel','q_dax','q_tax',...
    'inflexppoint_long','homeown','exphp_point','expint_sav','sl_major',...
    'sl_essential','sl_clothing','sl_entz','sl_mobility','sl_services',...
    'sl_holiday','sl_housing','sl_reserves','si_major','si_essential',...
    'si_clothing','si_entz','si_mobility','si_services','si_holiday',....
    'si_housing','si_reserves','f_nointerest','f_easy','f_short',...
    'eduschool','eduwork','hhchildren','hhinc','pinc',...
    'shop_groceries_nsing','shop_major_nsing','prep_meals_nsing',...
    'decide_finance_nsing','assets','debt_col',...
    'debt_nocol','age','citysize','prob_intqr','prob_md','incexp_intqr',...
    'incexp_md','exphp_intqr','exphp_md','fin_lit_subj','fin_lit_test',...
    'female','eastgerman','non_single','east1989','full_time','part_time',...
    'unemployed','retired','leave','homemaker','civil_servant',...
    'entrepreneur','refresher','nround','norent'};

% select vars that won't make it final
if strcmp(t,'old')
    rm = {'fin_lit_subj','fin_lit_test','assets','debt_col','debt_nocol',...
        'exphp_intqr','exphp_md','sl_major','sl_essential','sl_clothing',...
        'sl_entz','sl_mobility','sl_services','sl_holiday','sl_housing',...
        'sl_reserves','incexp_intqr','incexp_md','homeown','norent',...
        'q_growth','q_fuel','q_dax','q_tax','inflexppoint_long','homeown',...
        'exphp_point','expint_sav','si_major','si_essential','si_clothing',...
        'si_entz','si_mobility','si_services','si_holiday','si_housing',...
        'si_reserves','f_short','shop_major','prep_meals',...
        'shop_groceries_nsing','shop_major_nsing','prep_meals_nsing','decide_finance_nsing',...
        'decide_finance','prob_md','east1989','full_time','part_time',...
        'leave','homemaker','civil_servant','entrepreneur','norent'};
     rm_fin = {'assets','debt_col','debt_nocol',...
        'exphp_intqr','exphp_md','sl_major','sl_essential','sl_clothing',...
        'sl_entz','sl_mobility','sl_services','sl_holiday','sl_housing',...
        'sl_reserves','incexp_intqr','incexp_md','homeown','norent',...
        'q_growth','q_fuel','q_dax','q_tax','inflexppoint_long','homeown',...
        'exphp_point','expint_sav','si_major','si_essential','si_clothing',...
        'si_entz','si_mobility','si_services','si_holiday','si_housing',...
        'si_reserves','f_short','shop_major','prep_meals',...
        'shop_groceries_nsing','shop_major_nsing','prep_meals_nsing','decide_finance_nsing',...
        'decide_finance','prob_md','east1989','full_time','part_time',...
        'leave','homemaker','civil_servant','entrepreneur','norent'};
elseif strcmp(t,'female_only')
    rm = {'pessimist','q_unemployment','q_rent','q_lending','q_interest',...
    'q_inflation','q_property','q_growth','q_fuel','q_dax','q_tax',...
    'inflexppoint_long','homeown','exphp_point','expint_sav','sl_major',...
    'sl_essential','sl_clothing','sl_entz','sl_mobility','sl_services',...
    'sl_holiday','sl_housing','sl_reserves','si_major','si_essential',...
    'si_clothing','si_entz','si_mobility','si_services','si_holiday',....
    'si_housing','si_reserves','f_nointerest','f_easy','f_short',...
    'eduschool','eduwork','hhchildren','hhinc','pinc',...
    'shop_groceries_nsing','shop_major_nsing','prep_meals_nsing',...
    'decide_finance_nsing','assets','debt_col',...
    'debt_nocol','age','citysize','prob_intqr','prob_md','incexp_intqr',...
    'incexp_md','exphp_intqr','exphp_md','fin_lit_subj','fin_lit_test',...
    'eastgerman','non_single','east1989','full_time','part_time',...
    'unemployed','retired','leave','homemaker','civil_servant',...
    'entrepreneur','refresher','nround','norent'};
else
    rm = {'fin_lit_subj','fin_lit_test','assets','debt_col','debt_nocol',...
        'exphp_intqr','exphp_md','sl_major','sl_essential','sl_clothing',...
        'sl_entz','sl_mobility','sl_services','sl_holiday','sl_housing',...
        'sl_reserves','incexp_intqr','incexp_md','homeown','norent',...
        'exphp_intqr','exphp_md','inflexppoint_long','f_short','prob_md',...
        'leave','homemaker','civil_servant','entrepreneur'};
    % select vars that won't make it final but preserve fin literacy
    rm_fin = {'assets','debt_col','debt_nocol',...
        'exphp_intqr','exphp_md','sl_major','sl_essential','sl_clothing',...
        'sl_entz','sl_mobility','sl_services','sl_holiday','sl_housing',...
        'sl_reserves','incexp_intqr','incexp_md','homeown','norent'...
        'exphp_intqr','exphp_md','inflexppoint_long','f_short','prob_md',...
        'leave','homemaker','civil_servant','entrepreneur'};
end





% ---------------------
% Set working directory
% ---------------------
% The code below will traverse the path upwards until it finds the root folder of the project.

cd(fullfile(PROJECT_DIR, PROJECT))
addpath(fullfile(PROJECT_DIR, PROJECT,'empirical','1_code','functions'))

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

% --- combine in one table
R = [];
id = [];
wave = [];
y = [];
year = [];
month = [];

if strcmp(t,'female_only')
    vars = vars(~contains(vars,rm));
end

for i=w

    if i<10
        temp_var = strcat('w0',num2str(i));
    elseif i>=10
        temp_var = strcat('w',num2str(i));
    end

    % compile table of selected variables
    r = [];
    for ii = vars % all variables we actually take
        try r = [r,W.(temp_var)(:,ii)]; end
    end

    if width(r) == numel(vars) % if the wave contains all relevant variables
        % stack across selected waves
        R = [R;r];

        % stack also id, wave and dependent variable
        id = [id;W.(temp_var).id];
        wave = [wave;W.(temp_var).wave];
        y = [y;W.(temp_var).y];
        year = [year;W.(temp_var).year];
        month = [month;W.(temp_var).month];
    end
end

R.y = y; 

if strcmp(t,'female_only')
    writetable([R table(year,'VariableNames',{'year'}) table(month,'VariableNames',{'month'})],fullfile(pipeline, 'out', t,'T.csv'))
    return
end

R.id = id;
R.wave = wave;

% --- Save full data to pipeline folder -- 
save(fullfile(pipeline, 'out', 'R_full.mat'),'R',"w","NAME","pipeline",'PROJECT','PROJECT_DIR')



% ------------
%% Base Table
% ------------
% this has to be run in any case but we can build on

% clear all nonusable responses
T = removevars(R,rm);
T = removevars(R,{'id','wave','y'});

r=[];
for i = [-9999 -9998 -9997 -9996 -5555 -6666 9999 9998 9997 9996 5555 6666]
    [row,~]=find([table2array(T) y]==i); % drop out
    r = [r;row];     
    clear row
end

u = unique(r);
clear r


% same but including fin lit
T_fin = removevars(R,rm_fin);
T_fin = removevars(T_fin,{'id','wave','y'});
y_fin = y;
id_fin = id;
wave_fin = wave;

r=[];
for i = [-9999 -9998 -9997 -5555 -6666 9999 9998 9997 5555 6666]
    [row,~]=find([table2array(T_fin) y_fin]==i); % drop out
    r = [r;row];     
    clear row
end

u_fin = unique(r);


% remove missing vals
T(u,:)=[];
y(u)=[];
wave(u)=[];
id(u)=[];
lny = log(y-min(y)+0.1);


T_fin(u_fin,:)=[];
y_fin(u_fin)=[];
wave_fin(u_fin)=[];
id_fin(u_fin)=[];
lny_fin = log(y_fin-min(y_fin)+0.1);


clear r

% --- time dummy

% redefine w as vector of all included waves
w = unique(wave);
D = table;
for n = 1:numel(w)-1
    i = w(n);
    if i<10
        temp_var = strcat('w0',num2str(i));
    elseif i>=10
        temp_var = strcat('w',num2str(i));
    end
    d = double(wave==i);
    D.(temp_var) = d;
end

T = [T,D];

% redefine w as vector of all included waves
w = unique(wave_fin);
D = table;
for n = 1:numel(w)-1
    i = w(n);
    if i<10
        temp_var = strcat('w0',num2str(i));
    elseif i>=10
        temp_var = strcat('w',num2str(i));
    end
    d = double(wave_fin==i);
    D.(temp_var) = d;
end

T_fin = [T_fin,D];



% -----------
%% Interaction Terms Table
% --- compile table without missing values


if strcmp(t,'int') % if we are in interaction term world
   vars_nf = vars(~strcmp(vars,'female'));
   vars_nf_fin  = vars(~strcmp(vars,'female'));
   % create vector of rm that does not include female
   for ii = rm
        vars_nf = vars_nf(~strcmp(vars_nf,ii));
   end
   for ii = rm_fin
        vars_nf_fin = vars_nf_fin(~strcmp(vars_nf_fin,ii));
   end
   for i = vars_nf
       T = addvars(T,table2array(T(:,i)).*table2array(T(:,'female')),'NewVariableNames',strcat(i,'_fem'));
   end
   for i = vars_nf_fin
       T_fin = addvars(T_fin,table2array(T_fin(:,i)).*table2array(T_fin(:,'female')),'NewVariableNames',strcat(i,'_fem'));
   end
end


% -----------
%% No controls

% --- compile table without all three

if strcmp(t,'demo_only') % if we are in interaction term world
   T = removevars(T,{'pessimist','shop_groceries_nsing','shop_major_nsing','prep_meals_nsing','decide_finance_nsing','non_single','prob_intqr','nround','refresher','f_easy','f_nointerest'});
   T_fin = removevars(T_fin,{'pessimist','shop_groceries_nsing','shop_major_nsing','prep_meals_nsing','decide_finance_nsing','non_single','prob_intqr','nround','refresher','f_easy','f_nointerest'});
end

% --- compile table without sentiment

if strcmp(t,'no_pessimist') % if we are in interaction term world
   T = removevars(T,'pessimist');
   T_fin = removevars(T_fin,'pessimist');
end

% --- compile table without qualitative block

if strcmp(t,'no_quali') % if we are in interaction term world
   T = removevars(T,{'q_dax','q_unemployment','q_rent','q_lending','q_interest','q_inflation','q_property','q_growth','q_fuel','q_tax'});
   T_fin = removevars(T_fin,{'q_dax','q_unemployment','q_rent','q_lending','q_interest','q_inflation','q_property','q_growth','q_fuel','q_tax'});
end

% --- compile table without shopintent

if strcmp(t,'no_shopintent') % if we are in interaction term world
   T = removevars(T,{'si_clothing','si_entz','si_essential','si_holiday','si_housing','si_major','si_mobility','si_reserves','si_services'});
   T_fin = removevars(T_fin,{'si_clothing','si_entz','si_essential','si_holiday','si_housing','si_major','si_mobility','si_reserves','si_services'});
end


% --- compile table without feedback

if strcmp(t,'no_feedback') % if we are in interaction term world
   T = removevars(T,{'f_easy','f_nointerest'});
   T_fin = removevars(T_fin,{'f_easy','f_nointerest'});
end

% --- compile table without education

if strcmp(t,'no_edu') % if we are in interaction term world
   T = removevars(T,{'eduschool','eduwork'});
   T_fin = removevars(T_fin,{'eduschool','eduwork'});
end

% --- compile table without income

if strcmp(t,'no_inc') % if we are in interaction term world
   T = removevars(T,{'hhinc','pinc'});
   T_fin = removevars(T_fin,{'hhinc','pinc'});
end

% --- compile table without hh roles

if strcmp(t,'no_hhroles') % if we are in interaction term world
   T = removevars(T,{'shop_groceries_nsing','shop_major_nsing','prep_meals_nsing','decide_finance_nsing','non_single'});
   T_fin = removevars(T_fin,{'shop_groceries_nsing','shop_major_nsing','prep_meals_nsing','decide_finance_nsing','non_single'});
end

% --- compile table without uncertainty measures

if strcmp(t,'no_uncertainty') % if we are in interaction term world
   T = removevars(T,{'prob_intqr','nround','refresher','f_easy','f_nointerest'});
   T_fin = removevars(T_fin,{'prob_intqr','nround','refresher','f_easy','f_nointerest'});
end

% --- compile table without employment measures

if strcmp(t,'no_employ') % if we are in interaction term world
   T = removevars(T,{'part_time','full_time','unemployed','retired'});
   T_fin = removevars(T_fin,{'part_time','full_time','unemployed','retired'});
end

% --- compile table without geographic measures

if strcmp(t,'no_geo') % if we are in interaction term world
   T = removevars(T,{'citysize','east1989','eastgerman'});
   T_fin = removevars(T_fin,{'citysize','east1989','eastgerman'});
end


% ----------
% Saving
% ----------

save(fullfile(pipeline, 'out',t, 'T_cleaned.mat'),'T','y','lny','wave','id','T_fin','y_fin','lny_fin','wave_fin','id_fin','w',"NAME","pipeline",'PROJECT','PROJECT_DIR')
writetable([T table(y,'VariableNames',{'y'}) table(wave,'VariableNames',{'wave'}) table(id,'VariableNames',{'id'})],fullfile(pipeline, 'out',t, 'T.csv'))
writetable([T_fin table(y_fin,'VariableNames',{'y'}) table(wave_fin,'VariableNames',{'wave'}) table(id_fin,'VariableNames',{'id'})],fullfile(pipeline, 'out',t, 'T_fin.csv'))

% ----------
% Leftovers
% ----------
%% Here you leave any code snippets or temporary code that you don't need but don't want to delete just yet


% --- transform some variable in log

% T.log_sl_major = log(T.sl_major+1);
% T.log_sl_essential = log(T.sl_essential+1);
% T.log_sl_clothing = log(T.sl_clothing+1);
% T.log_sl_entz = log(T.sl_entz+1);
% T.log_sl_mobility = log(T.sl_mobility+1);
% T.log_sl_services = log(T.sl_services+1);
% T.log_sl_holiday = log(T.sl_holiday+1);
% T.log_sl_housing = log(T.sl_housing+1);
% T.log_sl_reserves = log(T.sl_reserves+1);
% T.log_assets = log(abs(T.assets+1));
% T.log_debt_col = log(T.debt_col+1);
% T.log_debt_nocol = log(T.debt_nocol+1);
% T.log_incexp_intqr = log(T.incexp_intqr+1);

% T = removevars(T,{'sl_major','sl_essential','sl_clothing','sl_entz','sl_mobility','sl_services','sl_holiday','sl_housing','sl_reserves'});
% T = removevars(T,{'assets','debt_col','debt_nocol'});
% T = removevars(T,{'incexp_intqr','incexp_md'});


% filter out vars with no variance
% variance = var(table2array(T));
% T = removevars(T,T.Properties.VariableNames(variance==0));
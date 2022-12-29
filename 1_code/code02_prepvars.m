%------------
%% Introduction
% ------------

% This file prepares all used variables such that they can be used in a
% regression. In the first part, some variables are dealth with
% individually. It is advised to run these codes only once per variable and
% wave. The main section can be run several times and runs fairly quick.

clear
NAME = 'code02_prepvars.m';
PROJECT = 'EmpiricalGenderGap';
PROJECT_DIR = 'D:\Lovisa\Studium\Oxford\Department of Economics\DPhil';

% ------------
%% Preamble
% ------------

% -- Load data from another pipeline folder --

% if you have run it for some sections before can load them in like this:
% load(fullfile('empirical', '2_pipeline', 'code02_prepvars.m', 'out', 'W.mat'),'W')
% W_2_15_25_33 = W;
% w = [2:15 25:33];
load(fullfile('empirical', '2_pipeline', 'code01_load.m', 'out', 'W.mat'),'W')

% if necessary, replace already existing ones
% for i=w
% 
%     if i<10
%         temp_var = strcat('w0',num2str(i));
%     elseif i>=10
%         temp_var = strcat('w',num2str(i));
%     end
% 
%     W.(temp_var) = W_2_15_25_33.(temp_var);
% 
% end


% --------
% Settings
% --------
% Any settings go here

% which waves
w = 2:33;

% which vars
vars = {'intqr','inflexppoint_long','incexp','exphp_prob','profession','edu','mainshopper','netwealth','fin_lit'};

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


% ---------
%% Prep some specific vars
% ---------

% -- intqr

if sum(strcmp('intqr',vars))==1

    innerbinedges = [-12 -8 -4 -2 0 2 4 8 12];
    allt=-12:1:12;

    for i=13:max(w)
            
    if i<10
        temp_var = strcat('w0',num2str(i));
    elseif i>=10
        temp_var = strcat('w',num2str(i));
    end

    
    Xt=[W.(temp_var).infexprob_a W.(temp_var).infexprob_b W.(temp_var).infexprob_c W.(temp_var).infexprob_d W.(temp_var).infexprob_e W.(temp_var).infexprob_f W.(temp_var).infexprob_g W.(temp_var).infexprob_h W.(temp_var).infexprob_i W.(temp_var).infexprob_j];
    [mn,md,intqr] = probabilistic_funcfit(Xt,innerbinedges,allt);
    W.(temp_var).prob_mn=mn;
    W.(temp_var).prob_md=md;
    W.(temp_var).prob_intqr=intqr;

    end
end



% --- quantitative long term inflation
% if assume they are constant we want to see if they answer the question in
% the next month and if so we impute back

if sum(strcmp('inflexppoint_long',vars))==1

    for i=w

        if i<10
            temp_var = strcat('w0',num2str(i));
        elseif i>=10
            temp_var = strcat('w',num2str(i));
        end


        % if variable is contained in this wave
        if sum(strcmp('inflexppoint_long',W.(temp_var).Properties.VariableNames))==1
            origin = floor(W.(temp_var).id/100000); % find first wave
            for ii = 1:height(W.(temp_var))
                if W.(temp_var).inflexppoint_long(ii) == -6666 && origin(ii) ~= i % if is not asked this question
                    range = origin(ii):i-1;

                    for iii = range % go through all past waves they were in

                        if iii < 10
                            temp_var_iii = strcat('w0',num2str(iii));
                        elseif iii >= 10
                            temp_var_iii = strcat('w',num2str(iii));
                        end

                        try
                            sols(iii-origin(ii)+1) = W.(temp_var_iii).inflexppoint_long(W.(temp_var_iii).id == W.(temp_var).id(ii));
                        catch
                            sols(iii-origin(ii)+1) = -6666;
                        end
                    end

                    if sum(sols>=0)~=0
                        sols = sols(sols>=0);
                        W.(temp_var).inflexppoint_long(ii) = sols(end); % give highest non missing response if possible
                    end
                    clear sols iii range temp_var_iii
                end
            end
        end
    end
end

% -- incexp

if sum(strcmp('incexp',vars))==1

    innerbinedges = [-2000 -1500 -1000 -500 -250 0 250 500 1000 1500 2000];
    allt=-2000:250:2000;

    for i=w
            
    if i<10
        temp_var = strcat('w0',num2str(i));
    elseif i>=10
        temp_var = strcat('w',num2str(i));
    end

    
    try Xt=[W.(temp_var).incexp_a W.(temp_var).incexp_b W.(temp_var).incexp_c W.(temp_var).incexp_d W.(temp_var).incexp_e W.(temp_var).incexp_f W.(temp_var).incexp_g W.(temp_var).incexp_h W.(temp_var).incexp_i W.(temp_var).incexp_j W.(temp_var).incexp_k W.(temp_var).incexp_l];
    [mn,md,intqr] = probabilistic_funcfit(Xt,innerbinedges,allt);
    W.(temp_var).incexp_mn=mn;
    W.(temp_var).incexp_md=md;
    W.(temp_var).incexp_intqr=intqr;
    end

    end
end

% --- exphp
if sum(strcmp('exphp_prob',vars))==1

    innerbinedges = [-12 -8 -4 -2 0 2 4 8 12];
    allt=-12:1:12;

    for i=w

        if i<10
            temp_var = strcat('w0',num2str(i));
        elseif i>=10
            temp_var = strcat('w',num2str(i));
        end


        try Xt=[W.(temp_var).exphp_prob_a W.(temp_var).exphp_prob_b W.(temp_var).exphp_prob_c W.(temp_var).exphp_prob_d W.(temp_var).exphp_prob_e W.(temp_var).exphp_prob_f W.(temp_var).exphp_prob_g W.(temp_var).exphp_prob_h W.(temp_var).exphp_prob_i W.(temp_var).exphp_prob_j];
            [mn,md,intqr] = probabilistic_funcfit(Xt,innerbinedges,allt);
            W.(temp_var).exphp_mn=mn;
            W.(temp_var).exphp_md=md;
            W.(temp_var).exphp_intqr=intqr; 
        end

    end
end


% --- education
% if assume they are constant we want to see if they answer the question in
% the next month and if so we impute back

if sum(strcmp('edu',vars))==1

    for i=w

        if i<10
            temp_var = strcat('w0',num2str(i));
        elseif i>=10
            temp_var = strcat('w',num2str(i));
        end


        % if variable is contained in this wave
        if sum(strcmp('eduschool',W.(temp_var).Properties.VariableNames))==1
            origin = floor(W.(temp_var).id/100000); % find first wave
            for ii = 1:height(W.(temp_var))
                if W.(temp_var).eduschool(ii) == -6666 % if is not asked this question
                    if origin(ii)<10
                        temp_var_ii = strcat('w0',num2str(origin(ii)));
                    elseif origin(ii)>=10
                        temp_var_ii = strcat('w',num2str(origin(ii)));
                    end
                    if W.(temp_var_ii).eduschool(W.(temp_var_ii).id == W.(temp_var).id(ii)) ~= 1 % if back then was not in school
                        try W.(temp_var).eduschool(ii) = W.(temp_var_ii).eduschool(W.(temp_var_ii).id == W.(temp_var).id(ii));
                        end
                    else
                        try W.(temp_var).eduschool(ii) = -9998; % if was at school back then we cannot know now
                        end
                    end
                end
            end

        end

        % if variable is contained in this wave
        if sum(strcmp('eduwork',W.(temp_var).Properties.VariableNames))==1
            origin = floor(W.(temp_var).id/100000); % find first wave
            for ii = 1:height(W.(temp_var))
                if W.(temp_var).eduwork(ii) == -6666 % if is not asked this question
                    if origin(ii)<10
                        temp_var_ii = strcat('w0',num2str(origin(ii)));
                    elseif origin(ii)>=10
                        temp_var_ii = strcat('w',num2str(origin(ii)));
                    end
                    if W.(temp_var_ii).eduwork(W.(temp_var_ii).id == W.(temp_var).id(ii)) ~= 1 % if back then was not in training
                        try W.(temp_var).eduwork(ii) = W.(temp_var_ii).eduwork(W.(temp_var_ii).id == W.(temp_var).id(ii));
                        end
                    else
                        try W.(temp_var).eduwork(ii) = -9998; % if was at school back then we cannot know now
                        end
                    end
                end
            end

        end

    end
end


% --- profession branch is constant
if sum(strcmp('profession',vars))==1

    for i=w

        if i<10
            temp_var = strcat('w0',num2str(i));
        elseif i>=10
            temp_var = strcat('w',num2str(i));
        end


        % if variable is contained in this wave
        if sum(strcmp('profession',W.(temp_var).Properties.VariableNames))==1
            origin = floor(W.(temp_var).id/100000); % find first wave
            for ii = 1:height(W.(temp_var))
                if W.(temp_var).profession(ii) == -6666 % if is not asked this question
                    if origin(ii)<10
                        temp_var_ii = strcat('w0',num2str(origin(ii)));
                    elseif origin(ii)>=10
                        temp_var_ii = strcat('w',num2str(origin(ii)));
                    end
                    try W.(temp_var).profession(ii) = W.(temp_var_ii).profession(W.(temp_var_ii).id == W.(temp_var).id(ii));
                    end

                end
            end
        end

    end
end


% --- mainshopper
if sum(strcmp('mainshopper',vars))==1

    for i=w

        if i<10
            temp_var = strcat('w0',num2str(i));
        elseif i>=10
            temp_var = strcat('w',num2str(i));
        end


        % if variable is contained in this wave
        if sum(strcmp('mainshopper_a',W.(temp_var).Properties.VariableNames))==1

            % if it hasn't been done before
            if sum(ismember(unique(W.(temp_var).mainshopper_a),0))==0
                W.(temp_var).mainshopper_a = W.(temp_var).mainshopper_a+1;
                W.(temp_var).mainshopper_a(W.(temp_var).mainshopper_a==4) = 0; % doesn't do at all
                W.(temp_var).mainshopper_a(W.(temp_var).mainshopper_a==3) = 1; % do together
                W.(temp_var).mainshopper_a(W.(temp_var).mainshopper_a==-6665) = -6666; % correct error code
            end

            W.(temp_var).mainshopper_a(W.(temp_var).hhsize==1) = 2; % those who live alone do all

            origin = floor(W.(temp_var).id/100000); % find first wave
            for ii = 1:height(W.(temp_var))
                if W.(temp_var).mainshopper_a(ii) == -6666 && origin(ii) ~= i % if is not asked this question
                    range = origin(ii):i-1;

                    for iii = range % go through all past waves they were in

                        if iii < 10
                            temp_var_iii = strcat('w0',num2str(iii));
                        elseif iii >= 10
                            temp_var_iii = strcat('w',num2str(iii));
                        end

                        try
                            sols(iii-origin(ii)+1) = W.(temp_var_iii).mainshopper_a(W.(temp_var_iii).id == W.(temp_var).id(ii));
                        catch
                            sols(iii-origin(ii)+1) = -6666;
                        end
                    end

                    if sum(sols>=0)~=0
                        sols = sols(sols>=0);
                        W.(temp_var).mainshopper_a(ii) = sols(end); % give highest non missing response if possible
                    end
                    clear sols

                end
            end
        end


        if sum(strcmp('mainshopper_b',W.(temp_var).Properties.VariableNames))==1

            % if it hasn't been done before
            if sum(ismember(unique(W.(temp_var).mainshopper_b),0))==0
                W.(temp_var).mainshopper_b = W.(temp_var).mainshopper_b+1;
                W.(temp_var).mainshopper_b(W.(temp_var).mainshopper_b==4) = 0; % doesn't do at all
                W.(temp_var).mainshopper_b(W.(temp_var).mainshopper_b==3) = 1; % do together
                W.(temp_var).mainshopper_b(W.(temp_var).mainshopper_b==-6665) = -6666; % correct error code
            end

            W.(temp_var).mainshopper_b(W.(temp_var).hhsize==1) = 2; % those who live alone do all

            origin = floor(W.(temp_var).id/100000); % find first wave
            for ii = 1:height(W.(temp_var))
                if W.(temp_var).mainshopper_b(ii) == -6666 && origin(ii) ~= i % if is not asked this question (remember plus one)
                    range = origin(ii):i-1;

                    for iii = range % go through all past waves they were in

                        if iii < 10
                            temp_var_iii = strcat('w0',num2str(iii));
                        elseif iii >= 10
                            temp_var_iii = strcat('w',num2str(iii));
                        end

                        try
                            sols(iii-origin(ii)+1) = W.(temp_var_iii).mainshopper_b(W.(temp_var_iii).id == W.(temp_var).id(ii));
                        catch
                            sols(iii-origin(ii)+1) = -6666;
                        end
                    end

                    if sum(sols>=0)~=0
                        sols = sols(sols>=0);
                        W.(temp_var).mainshopper_b(ii) = sols(end); % give highest non missing response if possible
                    end
                    clear sols

                end
            end
        end


        if sum(strcmp('mainshopper_c',W.(temp_var).Properties.VariableNames))==1

            % if it hasn't been done before
            if sum(ismember(unique(W.(temp_var).mainshopper_c),0))==0
                W.(temp_var).mainshopper_c = W.(temp_var).mainshopper_c+1;
                W.(temp_var).mainshopper_c(W.(temp_var).mainshopper_c==4) = 0; % doesn't do at all
                W.(temp_var).mainshopper_c(W.(temp_var).mainshopper_c==3) = 1; % do together
                W.(temp_var).mainshopper_c(W.(temp_var).mainshopper_c==-6665) = -6666; % correct error code
            end

            W.(temp_var).mainshopper_c(W.(temp_var).hhsize==1) = 2; % those who live alone do all

            origin = floor(W.(temp_var).id/100000); % find first wave
            for ii = 1:height(W.(temp_var))
                if W.(temp_var).mainshopper_c(ii) == -6666 && origin(ii) ~= i % if is not asked this question (remember plus one)
                    range = origin(ii):i-1;

                    for iii = range % go through all past waves they were in

                        if iii < 10
                            temp_var_iii = strcat('w0',num2str(iii));
                        elseif iii >= 10
                            temp_var_iii = strcat('w',num2str(iii));
                        end

                        try
                            sols(iii-origin(ii)+1) = W.(temp_var_iii).mainshopper_c(W.(temp_var_iii).id == W.(temp_var).id(ii));
                        catch
                            sols(iii-origin(ii)+1) = -6666;
                        end
                    end

                    if sum(sols>=0)~=0
                        sols = sols(sols>=0);
                        W.(temp_var).mainshopper_c(ii) = sols(end); % give highest non missing response if possible
                    end
                    clear sols

                end
            end
        end


        if sum(strcmp('mainshopper_d',W.(temp_var).Properties.VariableNames))==1

            % if it hasn't been done before
            if sum(ismember(unique(W.(temp_var).mainshopper_d),0))==0
                W.(temp_var).mainshopper_d = W.(temp_var).mainshopper_d+1;
                W.(temp_var).mainshopper_d(W.(temp_var).mainshopper_d==4) = 0; % doesn't do at all
                W.(temp_var).mainshopper_d(W.(temp_var).mainshopper_d==3) = 1; % do together
                W.(temp_var).mainshopper_d(W.(temp_var).mainshopper_d==-6665) = -6666; % correct error code
            end

            W.(temp_var).mainshopper_d(W.(temp_var).hhsize==1) = 2; % those who live alone do all

            origin = floor(W.(temp_var).id/100000); % find first wave
            for ii = 1:height(W.(temp_var))
                if W.(temp_var).mainshopper_d(ii) == -6666 && origin(ii) ~= i % if is not asked this question (remember plus one)
                    range = origin(ii):i-1;

                    for iii = range % go through all past waves they were in

                        if iii < 10
                            temp_var_iii = strcat('w0',num2str(iii));
                        elseif iii >= 10
                            temp_var_iii = strcat('w',num2str(iii));
                        end

                        try
                            sols(iii-origin(ii)+1) = W.(temp_var_iii).mainshopper_d(W.(temp_var_iii).id == W.(temp_var).id(ii));
                        catch
                            sols(iii-origin(ii)+1) = -6666;
                        end
                    end

                    if sum(sols>=0)~=0
                        sols = sols(sols>=0);
                        W.(temp_var).mainshopper_d(ii) = sols(end); % give highest non missing response if possible
                    end
                    clear sols iii range temp_var_iii
                end
            end
        end
    end
end

% --- netwealth part 1

if sum(strcmp('netwealth',vars))==1

    for i=w

        if i<10
            temp_var = strcat('w0',num2str(i));
        elseif i>=10
            temp_var = strcat('w',num2str(i));
        end

        % if we are in netwealth (no detail) world
        try W.(temp_var) = renamevars(W.(temp_var),'netwealth_a','assets');
            W.(temp_var).assets(W.(temp_var).assets==1) = 0;
            W.(temp_var).assets(W.(temp_var).assets==2) = 2500;
            W.(temp_var).assets(W.(temp_var).assets==3) = 5000;
            W.(temp_var).assets(W.(temp_var).assets==4) = 10000;
            W.(temp_var).assets(W.(temp_var).assets==5) = 25000;
            W.(temp_var).assets(W.(temp_var).assets==6) = 50000;
            W.(temp_var).assets(W.(temp_var).assets==7) = 75000;
            W.(temp_var).assets(W.(temp_var).assets==8) = 100000;
            W.(temp_var).assets(W.(temp_var).assets==9) = 250000;
            W.(temp_var).assets(W.(temp_var).assets==10) = 500000;
        end
        try W.(temp_var) = renamevars(W.(temp_var),'netwealth_b','debt_col');
            W.(temp_var).debt_col(W.(temp_var).debt_col==1) = 0;
            W.(temp_var).debt_col(W.(temp_var).debt_col==2) = 1;
            W.(temp_var).debt_col(W.(temp_var).debt_col==3) = 25000;
            W.(temp_var).debt_col(W.(temp_var).debt_col==4) = 50000;
            W.(temp_var).debt_col(W.(temp_var).debt_col==5) = 100000;
            W.(temp_var).debt_col(W.(temp_var).debt_col==6) = 150000;
            W.(temp_var).debt_col(W.(temp_var).debt_col==7) = 200000;
            W.(temp_var).debt_col(W.(temp_var).debt_col==8) = 300000;
            W.(temp_var).debt_col(W.(temp_var).debt_col==9) = 500000;
        end
        try W.(temp_var) = renamevars(W.(temp_var),'netwealth_c','debt_nocol');
            W.(temp_var).debt_nocol(W.(temp_var).debt_nocol==1) = 0;
            W.(temp_var).debt_nocol(W.(temp_var).debt_nocol==2) = 1;
            W.(temp_var).debt_nocol(W.(temp_var).debt_nocol==3) = 1000;
            W.(temp_var).debt_nocol(W.(temp_var).debt_nocol==4) = 2000;
            W.(temp_var).debt_nocol(W.(temp_var).debt_nocol==5) = 5000;
            W.(temp_var).debt_nocol(W.(temp_var).debt_nocol==6) = 10000;
            W.(temp_var).debt_nocol(W.(temp_var).debt_nocol==7) = 20000;
            W.(temp_var).debt_nocol(W.(temp_var).debt_nocol==8) = 40000;
        end

        % if we are in netwealth detail world
        try W.(temp_var).netwealth_detail_a(W.(temp_var).netwealth_detail_a==1) = 0;
            W.(temp_var).netwealth_detail_a(W.(temp_var).netwealth_detail_a==2) = 1;
            W.(temp_var).netwealth_detail_a(W.(temp_var).netwealth_detail_a==3) = 2500;
            W.(temp_var).netwealth_detail_a(W.(temp_var).netwealth_detail_a==4) = 5000;
            W.(temp_var).netwealth_detail_a(W.(temp_var).netwealth_detail_a==5) = 10000;
            W.(temp_var).netwealth_detail_a(W.(temp_var).netwealth_detail_a==6) = 25000;
            W.(temp_var).netwealth_detail_a(W.(temp_var).netwealth_detail_a==7) = 50000;
            W.(temp_var).netwealth_detail_a(W.(temp_var).netwealth_detail_a==8) = 100000;
            W.(temp_var).netwealth_detail_a(W.(temp_var).netwealth_detail_a==9) = 250000;
            W.(temp_var).netwealth_detail_a(W.(temp_var).netwealth_detail_a==10) = 500000;
        end
        try W.(temp_var).netwealth_detail_b(W.(temp_var).netwealth_detail_b==1) = 0;
            W.(temp_var).netwealth_detail_b(W.(temp_var).netwealth_detail_b==2) = 1;
            W.(temp_var).netwealth_detail_b(W.(temp_var).netwealth_detail_b==3) = 100000;
            W.(temp_var).netwealth_detail_b(W.(temp_var).netwealth_detail_b==4) = 200000;
            W.(temp_var).netwealth_detail_b(W.(temp_var).netwealth_detail_b==5) = 300000;
            W.(temp_var).netwealth_detail_b(W.(temp_var).netwealth_detail_b==6) = 400000;
            W.(temp_var).netwealth_detail_b(W.(temp_var).netwealth_detail_b==7) = 500000;
            W.(temp_var).netwealth_detail_b(W.(temp_var).netwealth_detail_b==8) = 750000;
            W.(temp_var).netwealth_detail_b(W.(temp_var).netwealth_detail_b==9) = 1000000;
            W.(temp_var).netwealth_detail_b(W.(temp_var).netwealth_detail_b==10) = 1500000;
        end
        try W.(temp_var).netwealth_detail_c(W.(temp_var).netwealth_detail_c==1) = 0;
            W.(temp_var).netwealth_detail_c(W.(temp_var).netwealth_detail_c==2) = 1;
            W.(temp_var).netwealth_detail_c(W.(temp_var).netwealth_detail_c==3) = 2500;
            W.(temp_var).netwealth_detail_c(W.(temp_var).netwealth_detail_c==4) = 5000;
            W.(temp_var).netwealth_detail_c(W.(temp_var).netwealth_detail_c==5) = 10000;
            W.(temp_var).netwealth_detail_c(W.(temp_var).netwealth_detail_c==6) = 25000;
            W.(temp_var).netwealth_detail_c(W.(temp_var).netwealth_detail_c==7) = 50000;
            W.(temp_var).netwealth_detail_c(W.(temp_var).netwealth_detail_c==8) = 100000;
            W.(temp_var).netwealth_detail_c(W.(temp_var).netwealth_detail_c==9) = 250000;
            W.(temp_var).netwealth_detail_c(W.(temp_var).netwealth_detail_c==10) = 500000;
        end
        try W.(temp_var).netwealth_detail_d(W.(temp_var).netwealth_detail_d==1) = 0;
            W.(temp_var).netwealth_detail_d(W.(temp_var).netwealth_detail_d==2) = 1;
            W.(temp_var).netwealth_detail_d(W.(temp_var).netwealth_detail_d==3) = 2500;
            W.(temp_var).netwealth_detail_d(W.(temp_var).netwealth_detail_d==4) = 5000;
            W.(temp_var).netwealth_detail_d(W.(temp_var).netwealth_detail_d==5) = 10000;
            W.(temp_var).netwealth_detail_d(W.(temp_var).netwealth_detail_d==6) = 25000;
            W.(temp_var).netwealth_detail_d(W.(temp_var).netwealth_detail_d==7) = 50000;
            W.(temp_var).netwealth_detail_d(W.(temp_var).netwealth_detail_d==8) = 100000;
            W.(temp_var).netwealth_detail_d(W.(temp_var).netwealth_detail_d==9) = 250000;
            W.(temp_var).netwealth_detail_d(W.(temp_var).netwealth_detail_d==10) = 500000;
        end
        try W.(temp_var).netwealth_detail_e(W.(temp_var).netwealth_detail_e==1) = 0;
            W.(temp_var).netwealth_detail_e(W.(temp_var).netwealth_detail_e==2) = 1;
            W.(temp_var).netwealth_detail_e(W.(temp_var).netwealth_detail_e==3) = 2500;
            W.(temp_var).netwealth_detail_e(W.(temp_var).netwealth_detail_e==4) = 5000;
            W.(temp_var).netwealth_detail_e(W.(temp_var).netwealth_detail_e==5) = 10000;
            W.(temp_var).netwealth_detail_e(W.(temp_var).netwealth_detail_e==6) = 25000;
            W.(temp_var).netwealth_detail_e(W.(temp_var).netwealth_detail_e==7) = 50000;
            W.(temp_var).netwealth_detail_e(W.(temp_var).netwealth_detail_e==8) = 100000;
            W.(temp_var).netwealth_detail_e(W.(temp_var).netwealth_detail_e==9) = 250000;
            W.(temp_var).netwealth_detail_e(W.(temp_var).netwealth_detail_e==10) = 500000;
        end
        try W.(temp_var).netwealth_detail_f(W.(temp_var).netwealth_detail_f==1) = 0;
            W.(temp_var).netwealth_detail_f(W.(temp_var).netwealth_detail_f==2) = 1;
            W.(temp_var).netwealth_detail_f(W.(temp_var).netwealth_detail_f==3) = 25000;
            W.(temp_var).netwealth_detail_f(W.(temp_var).netwealth_detail_f==4) = 50000;
            W.(temp_var).netwealth_detail_f(W.(temp_var).netwealth_detail_f==5) = 100000;
            W.(temp_var).netwealth_detail_f(W.(temp_var).netwealth_detail_f==6) = 150000;
            W.(temp_var).netwealth_detail_f(W.(temp_var).netwealth_detail_f==7) = 200000;
            W.(temp_var).netwealth_detail_f(W.(temp_var).netwealth_detail_f==8) = 300000;
            W.(temp_var).netwealth_detail_f(W.(temp_var).netwealth_detail_f==9) = 500000;
            W.(temp_var).netwealth_detail_f(W.(temp_var).netwealth_detail_f==10) = 750000;
        end
        try W.(temp_var).netwealth_detail_g(W.(temp_var).netwealth_detail_g==1) = 0;
            W.(temp_var).netwealth_detail_g(W.(temp_var).netwealth_detail_g==2) = 1;
            W.(temp_var).netwealth_detail_g(W.(temp_var).netwealth_detail_g==3) = 1000;
            W.(temp_var).netwealth_detail_g(W.(temp_var).netwealth_detail_g==4) = 2000;
            W.(temp_var).netwealth_detail_g(W.(temp_var).netwealth_detail_g==5) = 5000;
            W.(temp_var).netwealth_detail_g(W.(temp_var).netwealth_detail_g==6) = 10000;
            W.(temp_var).netwealth_detail_g(W.(temp_var).netwealth_detail_g==7) = 20000;
            W.(temp_var).netwealth_detail_g(W.(temp_var).netwealth_detail_g==8) = 40000;
            W.(temp_var).netwealth_detail_g(W.(temp_var).netwealth_detail_g==9) = 60000;
            W.(temp_var).netwealth_detail_g(W.(temp_var).netwealth_detail_g==10) = 100000;
        end

        if sum(strcmp('netwealth_detail_a',W.(temp_var).Properties.VariableNames))==1
            W.(temp_var).assets = W.(temp_var).netwealth_detail_a + W.(temp_var).netwealth_detail_b + W.(temp_var).netwealth_detail_c + W.(temp_var).netwealth_detail_d +W.(temp_var).netwealth_detail_e;
            W.(temp_var).debt_col = W.(temp_var).netwealth_detail_f;
            W.(temp_var).debt_nocol = W.(temp_var).netwealth_detail_g;
        end
    end
end

% --- financial literacy

if sum(strcmp('fin_lit',vars))==1

    % first create the variables in the waves they exits
    for i=w

        if i<10
            temp_var = strcat('w0',num2str(i));
        elseif i>=10
            temp_var = strcat('w',num2str(i));
        end

        % if we get subjective literacy question
        if sum(strcmp('financial_literacy_subj',W.(temp_var).Properties.VariableNames))==1
            % reduce by one number to have 0 knowledge, 1 a little...
            W.(temp_var).financial_literacy_subj(W.(temp_var).financial_literacy_subj>0) = W.(temp_var).financial_literacy_subj(W.(temp_var).financial_literacy_subj>0)-1;
            W.(temp_var) = renamevars(W.(temp_var),'financial_literacy_subj','fin_lit_subj');
            subj = temp_var;
        end

        % if we get three literacy questions
        if sum(strcmp('compound_interest',W.(temp_var).Properties.VariableNames))==1 && sum(strcmp('real_rates',W.(temp_var).Properties.VariableNames))==1  && sum(strcmp('risk_diversification',W.(temp_var).Properties.VariableNames))==1
            % create fin lit test variable
            W.(temp_var).fin_lit_test = zeros(height(W.(temp_var)),1);
            % add a point for each correct variable. Don't know and no
            % answer are treated same as wrong answer
            % compound interest
            W.(temp_var).fin_lit_test(W.(temp_var).compound_interest==1) = W.(temp_var).fin_lit_test(W.(temp_var).compound_interest==1)+1; % first option was correct
            % real_rates
            W.(temp_var).fin_lit_test(W.(temp_var).real_rates==3) = W.(temp_var).fin_lit_test(W.(temp_var).real_rates==3)+1; % third option was correct
            % risk_diversification
            W.(temp_var).fin_lit_test(W.(temp_var).risk_diversification==2) = W.(temp_var).fin_lit_test(W.(temp_var).risk_diversification==2)+1; % second option was correct
        end
        test = 'w25';
    end

    % then assume it is constant
    for i=w

        if i<10
            temp_var = strcat('w0',num2str(i));
        elseif i>=10
            temp_var = strcat('w',num2str(i));
        end

        % if we don't get subjective literacy question
        if sum(strcmp('fin_lit_subj',W.(temp_var).Properties.VariableNames))==0
            W.(temp_var).fin_lit_subj = -6666*ones(height(W.(temp_var)),1); % baseline is they haven't been asked question
            for ii = 1:height(W.(temp_var)) % for all observations
                if ismember(W.(temp_var).id(ii),W.(subj).id) % check if they participated in wave with subj lit question
                    W.(temp_var).fin_lit_subj(ii) = W.(subj).fin_lit_subj(W.(temp_var).id(ii)==W.(subj).id);
                end
            end
        end

        % if we don't get literacy test
        if sum(strcmp('fin_lit_test',W.(temp_var).Properties.VariableNames))==0
            W.(temp_var).fin_lit_test = -6666*ones(height(W.(temp_var)),1); % baseline is they haven't been asked question
            for ii = 1:height(W.(temp_var)) % for all observations
                if ismember(W.(temp_var).id(ii),W.(test).id) % check if they participated in wave with lit test score
                    W.(temp_var).fin_lit_test(ii) = W.(test).fin_lit_test(W.(temp_var).id(ii)==W.(test).id);
                end
            end
        end
    end
end


% ---------
%% Main code
% ---------

% --- standard formatting for all

for i=w

    if i<10
        temp_var = strcat('w0',num2str(i));
    elseif i>=10
        temp_var = strcat('w',num2str(i));
    end

    % point forecast
    W.(temp_var).inflexppoint(W.(temp_var).infdef==2) = -W.(temp_var).inflexppoint(W.(temp_var).infdef==2);

    % female dummy
    W.(temp_var).female = W.(temp_var).gender-1;
    W.(temp_var).female(W.(temp_var).gender==3) = -9999;

    % east german dummy
    W.(temp_var).eastgerman(W.(temp_var).region~=4) = 0;
    W.(temp_var).eastgerman(W.(temp_var).region==4) = 1;

    % non-single dummy
    W.(temp_var).non_single = double(W.(temp_var).hhsize>1);

    % children
    W.(temp_var).hhchildren(W.(temp_var).hhsize==1) = 0;

    % personal income
    W.(temp_var).pinc(W.(temp_var).hhsize==1) = W.(temp_var).hhinc(W.(temp_var).hhsize==1);

    % grew up in east
    W.(temp_var).east1989 = double(W.(temp_var).eastwest1989==1);

    % eduschool
    W.(temp_var).eduschool(W.(temp_var).eduschool==8) = 0; % no school leaving certificate and not in school
    W.(temp_var).eduschool(W.(temp_var).eduschool==7) = -9998; % unidentifiable

    % eduschool
    W.(temp_var).eduwork(W.(temp_var).eduschool==1) = 0; % has not left school
    W.(temp_var).eduwork(W.(temp_var).eduwork==10) = 0; % is not doing any training
    W.(temp_var).eduwork(W.(temp_var).eduwork==9) = -9998; % unidentifiable

    % employ
    W.(temp_var).full_time = double(W.(temp_var).employ == 1);
    try W.(temp_var).full_time(W.(temp_var).employ_f == 1) = 1; end
    W.(temp_var).part_time = double(W.(temp_var).employ == 2);
    try W.(temp_var).part_time(W.(temp_var).employ_f == 2) = 1; end
    W.(temp_var).unemployed = double(W.(temp_var).employ == 5);
    try W.(temp_var).unemployed(W.(temp_var).employ_f == 10) = 1; end
    W.(temp_var).retired = double(or(W.(temp_var).employ == 7,W.(temp_var).employ == 8));
    try W.(temp_var).retired(W.(temp_var).employ_f == 3) = 1; end
    W.(temp_var).leave = double(W.(temp_var).employ == 4);
    W.(temp_var).leave(W.(temp_var).employ == -6666) = -6666;
    W.(temp_var).homemaker = double(W.(temp_var).employ == 10);
    W.(temp_var).homemaker(W.(temp_var).employ == -6666) = -6666;

    % profession
    try W.(temp_var).civil_servant = double(W.(temp_var).profession == 3); 
        W.(temp_var).civil_servant(W.(temp_var).profession == -6666) = -6666;
    end
    try W.(temp_var).entrepreneur = double(or(W.(temp_var).profession == 4,W.(temp_var).profession == 5)); 
        W.(temp_var).entrepreneur(W.(temp_var).profession == -6666) = -6666;
    end

    % familystatus
    try W.(temp_var).married = double(or(W.(temp_var).familystatus == 1,W.(temp_var).familystatus == 2)); 
        W.(temp_var).married(W.(temp_var).familystatus == -6666) = -6666;
    end

    % Macroeconomic expectations qualitative
    try W.(temp_var) = renamevars(W.(temp_var),'expmacroquali_a','q_unemployment'); end
    try W.(temp_var) = renamevars(W.(temp_var),'expmacroquali_b','q_rent'); end
    try W.(temp_var) = renamevars(W.(temp_var),'expmacroquali_c','q_lending'); end
    try W.(temp_var) = renamevars(W.(temp_var),'expmacroquali_d','q_interest'); end
    try W.(temp_var) = renamevars(W.(temp_var),'expmacroquali_e','q_inflation'); end
    try W.(temp_var) = renamevars(W.(temp_var),'expmacroquali_f','q_property'); end
    try W.(temp_var) = renamevars(W.(temp_var),'expmacroquali_g','q_growth'); end
    try W.(temp_var) = renamevars(W.(temp_var),'expmacroquali_h','q_fuel'); end
    try W.(temp_var) = renamevars(W.(temp_var),'expmacroquali_i','q_dax'); end
    try W.(temp_var) = renamevars(W.(temp_var),'expmacroquali_x','q_tax'); end

    % Spend last month
    try W.(temp_var) = renamevars(W.(temp_var),'spentlastmon_a','sl_major'); end
    try W.(temp_var) = renamevars(W.(temp_var),'spentlastmon_b','sl_essential'); end
    try W.(temp_var) = renamevars(W.(temp_var),'spentlastmon_c','sl_clothing'); end
    try W.(temp_var) = renamevars(W.(temp_var),'spentlastmon_d','sl_entz'); end
    try W.(temp_var) = renamevars(W.(temp_var),'spentlastmon_e','sl_mobility'); end
    try W.(temp_var) = renamevars(W.(temp_var),'spentlastmon_f','sl_services'); end
    try W.(temp_var) = renamevars(W.(temp_var),'spentlastmon_g','sl_holiday'); end
    try W.(temp_var) = renamevars(W.(temp_var),'spentlastmon_h','sl_housing'); end
    try W.(temp_var) = renamevars(W.(temp_var),'spentlastmon_i','sl_reserves'); end

    % Spend intent
    try W.(temp_var) = renamevars(W.(temp_var),'spendintent_a','si_major');
        W.(temp_var).si_major(W.(temp_var).si_major==3)=-1;
        W.(temp_var).si_major(W.(temp_var).si_major==2)=0;
    end
    try W.(temp_var) = renamevars(W.(temp_var),'spendintent_b','si_essential');
        W.(temp_var).si_essential(W.(temp_var).si_essential==3)=-1;
        W.(temp_var).si_essential(W.(temp_var).si_essential==2)=0;
    end
    try W.(temp_var) = renamevars(W.(temp_var),'spendintent_c','si_clothing');
        W.(temp_var).si_clothing(W.(temp_var).si_clothing==3)=-1;
        W.(temp_var).si_clothing(W.(temp_var).si_clothing==2)=0;
    end
    try W.(temp_var) = renamevars(W.(temp_var),'spendintent_d','si_entz');
        W.(temp_var).si_entz(W.(temp_var).si_entz==3)=-1;
        W.(temp_var).si_entz(W.(temp_var).si_entz==2)=0;
    end
    try W.(temp_var) = renamevars(W.(temp_var),'spendintent_e','si_mobility');
        W.(temp_var).si_mobility(W.(temp_var).si_mobility==3)=-1;
        W.(temp_var).si_mobility(W.(temp_var).si_mobility==2)=0;
    end
    try W.(temp_var) = renamevars(W.(temp_var),'spendintent_f','si_services');
        W.(temp_var).si_services(W.(temp_var).si_services==3)=-1;
        W.(temp_var).si_services(W.(temp_var).si_services==2)=0;
    end
    try W.(temp_var) = renamevars(W.(temp_var),'spendintent_g','si_holiday');
        W.(temp_var).si_holiday(W.(temp_var).si_holiday==3)=-1;
        W.(temp_var).si_holiday(W.(temp_var).si_holiday==2)=0;
    end
    try W.(temp_var) = renamevars(W.(temp_var),'spendintent_h','si_housing');
        W.(temp_var).si_housing(W.(temp_var).si_housing==3)=-1;
        W.(temp_var).si_housing(W.(temp_var).si_housing==2)=0;
    end
    try W.(temp_var) = renamevars(W.(temp_var),'spendintent_i','si_reserves');
        W.(temp_var).si_reserves(W.(temp_var).si_reserves==3)=-1;
        W.(temp_var).si_reserves(W.(temp_var).si_reserves==2)=0;
    end

    % mood
    try W.(temp_var) = renamevars(W.(temp_var),'mood2021','pessimist'); end
    try W.(temp_var) = renamevars(W.(temp_var),'moodnext12months','pessimist'); end
    try W.(temp_var) = renamevars(W.(temp_var),'mood_currentyear','pessimist'); end

    % define refresher
    W.(temp_var).refresher = double(W.(temp_var).id < W.(temp_var).wave*100000);

    % do this only if has not been done before
    if sum(ismember(unique(W.(temp_var).homeown),0))==0
        % norent
        W.(temp_var).norent = double(or(W.(temp_var).homeown==3,W.(temp_var).homeown==4));
        W.(temp_var).norent(W.(temp_var).homeown==-6666) = -6666;

        % homeown
        W.(temp_var).homeown(W.(temp_var).homeown==1) = 0; % doesn't own any property
        W.(temp_var).homeown(W.(temp_var).homeown>1) = 1; % own property
    end

    % mainshopper
    try W.(temp_var) = renamevars(W.(temp_var),'mainshopper_a','shop_groceries'); end
    try W.(temp_var) = renamevars(W.(temp_var),'mainshopper_b','shop_major'); end
    try W.(temp_var) = renamevars(W.(temp_var),'mainshopper_c','prep_meals'); end
    try W.(temp_var) = renamevars(W.(temp_var),'mainshopper_d','decide_finance'); end

    % feedback
    try W.(temp_var) = renamevars(W.(temp_var),'qinterest','f_nointerest'); end
    try W.(temp_var) = renamevars(W.(temp_var),'qeasy','f_easy'); end
    try W.(temp_var) = renamevars(W.(temp_var),'qlong','f_short'); end

    % nround
    W.(temp_var).nround=double(mod(W.(temp_var).inflexppoint,5)~=0);
end





%% -- Save data to pipeline folder -- 


save(fullfile(pipeline, 'out', 'W.mat'),"pipeline","W","w","NAME","PROJECT","PROJECT_DIR")




% ----------
% Leftovers
% ----------
%% Here you leave any code snippets or temporary code that you don't need but don't want to delete just yet

% if we want to impute mood
for i=w
    if i<10
        temp_var = strcat('w0',num2str(i));
    elseif i>=10
        temp_var = strcat('w',num2str(i));
    end
    
    % set place where it is supposed to take mood from
    if i < 16
        o = 'w16';
    elseif i == 20
        o = 'w19';        
    elseif i == 22 || i == 23
        o = 'w21';
    elseif i > 25
        o = 'w25';
    end

    % if we don't get mood
    if sum(strcmp('pessimist',W.(temp_var).Properties.VariableNames))==0
        W.(temp_var).pessimist = -6666*ones(height(W.(temp_var)),1); % baseline is they haven't been asked question
        for ii = 1:height(W.(temp_var)) % for all observations
            if ismember(W.(temp_var).id(ii),W.(o).id) % check if they participated in wave with subj lit question
                W.(temp_var).pessimist(ii) = W.(o).pessimist(W.(temp_var).id(ii)==W.(o).id);
            end
        end
    end
end

save(fullfile(pipeline, 'out', 'W_pessimist.mat'),"pipeline","W","w","NAME","PROJECT","PROJECT_DIR")

for i=w
    if i<10
        temp_var = strcat('w0',num2str(i));
    elseif i>=10
        temp_var = strcat('w',num2str(i));
    end
    
    % set place where it is supposed to take mood from
    o = 'w16';

    % if we don't get shopper
    if sum(strcmp('shop_groceries',W.(temp_var).Properties.VariableNames))==0
        W.(temp_var).shop_groceries = -6666*ones(height(W.(temp_var)),1); % baseline is they haven't been asked question
        for ii = 1:height(W.(temp_var)) % for all observations
            if ismember(W.(temp_var).id(ii),W.(o).id) % check if they participated in wave with subj lit question
                W.(temp_var).shop_groceries(ii) = W.(o).shop_groceries(W.(temp_var).id(ii)==W.(o).id);
            end
        end
    end
    % if we don't get shopper
    if sum(strcmp('shop_major',W.(temp_var).Properties.VariableNames))==0
        W.(temp_var).shop_major = -6666*ones(height(W.(temp_var)),1); % baseline is they haven't been asked question
        for ii = 1:height(W.(temp_var)) % for all observations
            if ismember(W.(temp_var).id(ii),W.(o).id) % check if they participated in wave with subj lit question
                W.(temp_var).shop_major(ii) = W.(o).shop_major(W.(temp_var).id(ii)==W.(o).id);
            end
        end
    end
    % if we don't get shopper
    if sum(strcmp('prep_meals',W.(temp_var).Properties.VariableNames))==0
        W.(temp_var).prep_meals = -6666*ones(height(W.(temp_var)),1); % baseline is they haven't been asked question
        for ii = 1:height(W.(temp_var)) % for all observations
            if ismember(W.(temp_var).id(ii),W.(o).id) % check if they participated in wave with subj lit question
                W.(temp_var).prep_meals(ii) = W.(o).prep_meals(W.(temp_var).id(ii)==W.(o).id);
            end
        end
    end
    % if we don't get shopper
    if sum(strcmp('decide_finance',W.(temp_var).Properties.VariableNames))==0
        W.(temp_var).decide_finance = -6666*ones(height(W.(temp_var)),1); % baseline is they haven't been asked question
        for ii = 1:height(W.(temp_var)) % for all observations
            if ismember(W.(temp_var).id(ii),W.(o).id) % check if they participated in wave with subj lit question
                W.(temp_var).decide_finance(ii) = W.(o).decide_finance(W.(temp_var).id(ii)==W.(o).id);
            end
        end
    end
end

% include interaction variables with family for hhroles
for i=w
    if i<10
        temp_var = strcat('w0',num2str(i));
    elseif i>=10
        temp_var = strcat('w',num2str(i));
    end

    for ii = {'shop_groceries','shop_major','prep_meals','decide_finance'}
       W.(temp_var) = addvars(W.(temp_var),table2array(W.(temp_var)(:,ii)).*table2array(W.(temp_var)(:,'non_single')),'NewVariableNames',strcat(ii,'_nsing'));
    end
end

save(fullfile(pipeline, 'out', 'W_pesshop.mat'),"pipeline","W","w","NAME","PROJECT","PROJECT_DIR")
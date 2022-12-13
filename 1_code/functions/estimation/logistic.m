%% set directory
clc
clear

cd 'D:\Lovisa\Studium\Oxford\Department of Economics\Thesis\Code'
addpath 'D:\Lovisa\Studium\Oxford\Department of Economics\Thesis'
addpath 'D:\Lovisa\Studium\Oxford\Department of Economics\Thesis\Code\paneldata\estimation'
addpath 'D:\Lovisa\Studium\Oxford\Department of Economics\Thesis\Code\paneldata\util'
addpath 'D:\Lovisa\Studium\Oxford\Department of Economics\Thesis\Code\paneldata\tests'
addpath 'D:\Lovisa\Studium\Oxford\Department of Economics\Thesis\Code\paneldata\stafun'

load final_panel.mat

Final_work = [Final.prob_intqr Final.cert1 Final.qeasy];
Final_p_max = max(Final_work); % put these in gower_distfun
Final_p_min = min(Final_work);
save minmax.mat Final_p_max Final_p_min
k=2;

[idx_certainty,~,~,~,~,~] = kmedoids(Final_work,k,'Algorithm','pam','Distance',@gower_distfun);
i_cert=double(idx_certainty==2);
i_exp = double(Final.mainshopper_a==0);
i_sent = double(Final.pessimist>=2);
Final = removevars(Final,{'prob_intqr','cert1','qeasy','mainshopper_a','pessimist','coronadeal'});

clear B_exp B_cert B_sent
for i=1:numel(unique(wave))
    i
    w=unique(wave);
    w=w(i);
    [B_exp(:,i),dev_exp(:,i),stats_exp(:,i)] = mnrfit(table2array(Final(wave==w,:)),categorical(i_exp(wave==w,:)));
    [B_cert(:,i),dev_cert(:,i),stats_cert(:,i)] = mnrfit(table2array(Final(wave==w,:)),categorical(i_cert(wave==w,:)));
    [B_sent(:,i),dev_sent(:,i),stats_sent(:,i)] = mnrfit(table2array(Final(wave==w,:)),categorical(i_sent(wave==w,:)));
end
    
beta_exp = mean(B_exp(:,i),2);
beta_cert = mean(B_cert(:,i),2);
beta_sent = mean(B_sent(:,i),2);

se_exp = mean([stats_exp(1).se,stats_exp(2).se,stats_exp(3).se,stats_exp(4).se,stats_exp(5).se],2);
se_cert = mean([stats_cert(1).se,stats_cert(2).se,stats_cert(3).se,stats_cert(4).se,stats_cert(5).se],2);
se_sent = mean([stats_sent(1).se,stats_sent(2).se,stats_sent(3).se,stats_sent(4).se,stats_sent(5).se],2);

t_exp = mean([stats_exp(1).t,stats_exp(2).t,stats_exp(3).t,stats_exp(4).t,stats_exp(5).t],2);
t_cert = mean([stats_cert(1).t,stats_cert(2).t,stats_cert(3).t,stats_cert(4).t,stats_cert(5).t],2);
t_sent = mean([stats_sent(1).t,stats_sent(2).t,stats_sent(3).t,stats_sent(4).t,stats_sent(5).t],2);

p_exp = mean([stats_exp(1).p,stats_exp(2).p,stats_exp(3).p,stats_exp(4).p,stats_exp(5).p],2);
p_cert = mean([stats_cert(1).p,stats_cert(2).p,stats_cert(3).p,stats_cert(4).p,stats_cert(5).p],2);
p_sent = mean([stats_sent(1).p,stats_sent(2).p,stats_sent(3).p,stats_sent(4).p,stats_sent(5).p],2);

varnames_exp = ['CONST'; Final.Properties.VariableNames'];
varnames_cert = ['CONST'; Final.Properties.VariableNames'];
varnames_sent = ['CONST'; Final.Properties.VariableNames'];

reg_exp = table(varnames_exp,beta_exp,se_exp,t_exp,p_exp);
reg_sent = table(varnames_sent,beta_sent,se_sent,t_sent,p_sent);
reg_cert = table(varnames_cert,beta_cert,se_cert,t_cert,p_cert);

writetable(reg_exp,'D:\Lovisa\Studium\Oxford\Department of Economics\Thesis\reg_output_cluster.xlsx','Sheet','Logistic','WriteRowNames',true,'WriteVariableNames',true,'Range','B2');
writetable(reg_cert,'D:\Lovisa\Studium\Oxford\Department of Economics\Thesis\reg_output_cluster.xlsx','Sheet','Logistic','WriteRowNames',true,'WriteVariableNames',true,'Range','H2');
writetable(reg_sent,'D:\Lovisa\Studium\Oxford\Department of Economics\Thesis\reg_output_cluster.xlsx','Sheet','Logistic','WriteRowNames',true,'WriteVariableNames',true,'Range','N2');



%% alternative exclude refreshers

Final2=Final(Final.refresher==0,:);
Final2 = removevars(Final2,{'refresher'});

i_cert = i_cert(Final.refresher==0);
i_exp = i_exp(Final.refresher==0);
i_sent = i_sent(Final.refresher==0);

[beta_exp,dev_exp,stats_exp] = mnrfit(table2array(Final2),categorical(i_exp));
[beta_cert,dev_cert,stats_cert] = mnrfit(table2array(Final2),categorical(i_cert));
[beta_sent,dev_sent,stats_sent] = mnrfit(table2array(Final2),categorical(i_sent));

varnames_exp = ['CONST'; Final2.Properties.VariableNames'];
varnames_cert = ['CONST'; Final2.Properties.VariableNames'];
varnames_sent = ['CONST'; Final2.Properties.VariableNames'];

reg_exp = table(varnames_exp,beta_exp,stats_exp.se,stats_exp.t,stats_exp.p);
reg_sent = table(varnames_sent,beta_sent,stats_sent.se,stats_sent.t,stats_sent.p);
reg_cert = table(varnames_cert,beta_cert,stats_cert.se,stats_cert.t,stats_cert.p);

writetable(reg_exp,'D:\Lovisa\Studium\Oxford\Department of Economics\Thesis\reg_output_cluster.xlsx','Sheet','Logistic','WriteRowNames',true,'WriteVariableNames',true,'Range','B2');
writetable(reg_cert,'D:\Lovisa\Studium\Oxford\Department of Economics\Thesis\reg_output_cluster.xlsx','Sheet','Logistic','WriteRowNames',true,'WriteVariableNames',true,'Range','H2');
writetable(reg_sent,'D:\Lovisa\Studium\Oxford\Department of Economics\Thesis\reg_output_cluster.xlsx','Sheet','Logistic','WriteRowNames',true,'WriteVariableNames',true,'Range','N2');


ssres_exp = sum(stats_exp.resid.^2)


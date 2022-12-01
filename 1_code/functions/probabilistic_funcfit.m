function [mn,md,intqr] = probabilistic_funcfit(Xt,innerbinedges,allt)

% Fit distributions to the probabilistic answers as in Engelberg et al 2009
% If a respondent uses 1-2 bins we assume instead that the subjective 
% distribution has the shape of an isosceles triangle
% If instead a respondent uses 3 or more bins we assume that the
% subjective distribution is a member of the generalized Beta family

% Input: Xt is a matrix of observations n x bins 
% Output: mn is the mean, md is the median and intqr the interquartile
% range of the fitted sitribution

% create output vectors
mn = zeros(height(Xt),1);
md = zeros(height(Xt),1);
intqr = zeros(height(Xt),1);
Xt(Xt == -6666)=0;

% mark respondents unusable
[row,~]=find(Xt==-9999); % drop out
if ~isempty(row)
    mn(row,:) = -9999;
    md(row,:) = -9999;
    intqr(row,:) = -9999;
end
[row,~]=find(Xt==-9998); % no answer
if ~isempty(row)
    mn(row,:) = -9998;
    md(row,:) = -9998;
    intqr(row,:) = -9998;
end
[row,~]=find(Xt==-9997); % don't know
if ~isempty(row)
    mn(row,:) = -9997;
    md(row,:) = -9997;
    intqr(row,:) = -9997;
end
[row,~]=find(Xt==-5555); % coding error by Bbk
if ~isempty(row)
    mn(row,:) = -5555;
    md(row,:) = -5555;
    intqr(row,:) = -5555;
end

clear row

% count how many bins someone uses
nbin=zeros(height(Xt),1);
for i=1:height(Xt)
    nbin(i)=sum(Xt(i,:)>0);
end

% find observations with probability mass in non-contiguous bins
discon=zeros(height(Xt),1);
for i=1:height(Xt)
    if nbin(i)>1
        posmas=find(Xt(i,:));
        subseq=posmas(1):1:posmas(end);
        if ~isequal(posmas,subseq)
            discon(i)=1;
        end
    end
end
clear posmas subseq

% we cannot use those respondents here
mn(discon==1)=-5555;
md(discon==1)=-5555;
intqr(discon==1)=-5555;

% remove all those with only zero
mn(nbin==0)=-6666;
md(nbin==0)=-6666;
intqr(nbin==0)=-6666;

% treat infinite bins
infbin=zeros(height(Xt),1);
infbin(Xt(:,1)==100)=1;
infbin(Xt(:,end)==100)=1;

mn(infbin==1)=-5555;
md(infbin==1)=-5555;
intqr(infbin==1)=-5555;


% fit distributions and calculate values
for i=1:height(Xt)
    if intqr(i) == 0 % if we haven't already preassigned a value
    i
    if nbin(i)==1 % case 1: all mass in one bin
        if infbin(i)==0
        x=find(Xt(i,:)); % find out where probability mass is
        a=innerbinedges(x-1); % lower edge of bin
        c=innerbinedges(x); % upper edge of bin
        
        it = makedist('Triangular','a',a,'b',a+(c-a)/2,'c',c); % fit Triangular distribution
        
        mn(i) = mean(it);
        md(i) = median(it);
        intqr(i) = iqr(it);
        end
    elseif nbin(i)==2 % case 2: mass in two bins, fit isosceles triangle
        try
            
        if discon(i)==0
        x=find(Xt(i,:)); % find out where probability mass is
        v1=Xt(i,x(1)); % value in first bin
        v2=Xt(i,x(2)); % value in second bin
        
        if v1<v2 % we use value 2 fully and value 1 only partially

            f = (innerbinedges(x(2))-innerbinedges(x(1)))/v2*100; % full length of support
            a=innerbinedges(x(1))-(f-(innerbinedges(x(2))-innerbinedges(x(1)))); % lower edge of bin
            c=innerbinedges(x(2)); % upper edge of bin
                              
        elseif v1>v2
            
            f = (innerbinedges(x(1))-innerbinedges(x(1)-1))/v1*100; % full length of support
            a=innerbinedges(x(1)-1); % lower edge of bin
            c=innerbinedges(x(1))+(f-(innerbinedges(x(1))-innerbinedges(x(1)-1))); % upper edge of bin
 
        end
        
        it = makedist('Triangular','a',a,'b',a+(c-a)/2,'c',c); % fit Triangular distribution
        
        mn(i) = mean(it);
        md(i) = median(it);
        intqr(i) = iqr(it);
        end
        
        catch
        mn(i) = -5555;
        md(i) = -5555;
        intqr(i) = -5555;
        end
    
    elseif nbin(i)>=3 % case 3: more than 3 bins, fit generalised beta function
        
        if discon(i)==0
        
        F_t=zeros(numel(allt),1);
        for ii = 1:numel(allt)
            if ii==1
                F_t(ii) = Xt(i,ii)./100;
            elseif ii>1
                if ~ismember(allt(ii),innerbinedges)
                    F_t(ii) = F_t(ii-1);
                else 
                    F_t(ii) = F_t(ii-1) + Xt(i,allt(ii)==innerbinedges)./100;
                end
            end
        end

        
        fun = @(a)sum((beta_homemade(allt,a(1),a(2),min(allt),max(allt))-F_t).^2);
        a = fmincon(fun,[2,2],[],[],[],[],[0,0],[Inf,Inf]);
        
        mn(i)=(a(1)*max(allt)+a(2)*min(allt))/(a(1)+a(2));
        md(i)=((a(1)-1/3)/(a(1)+a(2)-2/3))*(max(allt)-min(allt))+min(allt);
        
        try
            q1=allt(find(beta_homemade(allt,a(1),a(2),min(allt),max(allt))>0.25,1)-1);
            q3=allt(find(beta_homemade(allt,a(1),a(2),min(allt),max(allt))>0.75,1)-1);
            intqr(i)=q3-q1;
        catch
           intqr(i)=-5555;
        end
        
        end
    end 
    end

    
end

end
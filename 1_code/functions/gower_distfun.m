function D2 = gower_distfun(ZI,ZJ)
% Gower distance function: distance is given by the weighted average of the
% partial dissimilarities across observations 

% interpret data as n (1xp) row vectors

% n is number of observations
% p is number of variables

% ZI is a 1-by-p vector containing a single observation.
% ZJ is an (n-1)-by-p matrix containing multiple observation. distfun must accept a matrix ZJ with an arbitrary number of observations.
% D2 is an p2-by-1 vector of distances, and D2(k) is the distance between observations ZI and ZJ(k,:).

p = size(ZI,2);

% calculate matrix of partial dissimilarity

load(fullfile('empirical\2_pipeline\cluster.m','out','minmax.mat')) % pre-saved vectors for minimum and maximum values of the p variables
MPD=abs(ZI-ZJ)./abs(Wmax-Wmin);

% calculate distance as weighted average

D2 = (1/p)*sum(MPD,2);

end
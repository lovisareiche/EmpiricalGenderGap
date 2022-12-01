function isti=istinvariant2(id,X,tol)

% Default tolerance
if nargin < 4
    tol = 1e-10;
end

db=[];
if istable(X)
    X=table2array(X);
end

for i=1:numel(id)
    
    if size(X(id==id(i),:),1)>1
        db=[db;i];
    end
end

istv=zeros(numel(db),size(X,2));

for i=1:numel(db)   
    
    istv(i,find(sum(abs(X(id==id(db(i)),:)-mean(X(id==id(db(i)),:),1)),1)~=0))=1;
    
end

isti=sum(istv,1)./size(istv,1)<tol;

end
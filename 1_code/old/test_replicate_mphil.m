F.id = id;
F.wave = wave;
F.y = y;

estPO = panel(F.id, F.wave, F.y, F(:,1:end-3), 'po','vartype','cluster','clusterid',F.id);
printPO=estdisp(estPO);

% then load from ols

f = table(id);
f.wave = wave;
i = ismember(table2array(f),[F.id F.wave],'rows');

sum(i)

T.id = id;
T.wave = wave;
T.y = y;
onlyT = T(~i,:);

i2 =  ismember([F.id F.wave],table2array(f),'rows');

onlyF = F(~i2,:);

estPO = panel(onlyT.id, onlyT.wave, onlyT.y, onlyT(:,1:end-3), 'po','vartype','cluster','clusterid',onlyT.id);
printPO=estdisp(estPO);

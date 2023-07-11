
estPO = panel(id, wave, y, removevars(T,{'expint_sav','inflexppoint_long'}), 'po','vartype','cluster','clusterid',id);
printPO=estdisp(estPO);
% gets female positive but not significant

estPO = panel(id, wave, y, removevars(T,{'si_major','si_essential','si_clothing','si_entz','si_mobility','si_services','si_holiday','si_housing','si_reserves'}), 'po','vartype','cluster','clusterid',id);
printPO=estdisp(estPO);
% female stays negative

estPO = panel(id, wave, y, removevars(T,{'shop_major','prep_meals','decide_finance'}), 'po','vartype','cluster','clusterid',id);
printPO=estdisp(estPO);
% marginally positive

estPO = panel(id, wave, y, removevars(T,{'part_time','full_time','leave','homemaker'}), 'po','vartype','cluster','clusterid',id);
printPO=estdisp(estPO);
% marginally positive

estPO = panel(id, wave, y, removevars(T,{'prob_md'}), 'po','vartype','cluster','clusterid',id);
printPO=estdisp(estPO);
% stays negative

estPO = panel(id, wave, y, removevars(T,{'east1989'}), 'po','vartype','cluster','clusterid',id);
printPO=estdisp(estPO);
% stays negative

estPO = panel(id, wave, y, removevars(T,{'q_growth','q_fuel','q_dax','q_tax'}), 'po','vartype','cluster','clusterid',id);
printPO=estdisp(estPO);
% marginally positive



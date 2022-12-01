function out = beta_homemade(t,a,b,l,r)

out=zeros(numel(t),1);
for i=1:numel(t)

    if t(i)<=l
        out(i) = 0;
    elseif l<t(i) && t(i)<=r
        fun=@(z) (((z-l).^(a-1)).*((r-z).^(b-1)))/((r-l).^(a+b-1));
        out(i) = (1/beta(a,b)) * integral(fun,l,t(i));
    elseif r<t(i)
        out(i) = 1;
    end
    
end

end

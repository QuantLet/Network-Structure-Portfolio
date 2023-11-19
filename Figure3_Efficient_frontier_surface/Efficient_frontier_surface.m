clc,clear
%% simulation for 465 stocks
rng default;
mu = zeros(465,1); Sigma = diag(ones(465,1));
X = mvnrnd(mu,Sigma,500);
covmat = cov(X);
cormat = corr(X);
v = diag(covmat);
n = length(covmat);
covmat_inv=diag(ones(1,n))/covmat;
[V,D]=eig(cormat-diag(ones(n,1)));
EC=V(:,1);EC=(EC-min(EC))/(max(EC)-min(EC));
ER=mean(X);ER=ER';

lambda=linspace(-0.3,0.3,20);
gamma=linspace(-0.3,0.3,20);
[Lambda,Gamma]=meshgrid(lambda,gamma);
theta1=sum(sum(covmat_inv));
theta2=sum(covmat_inv*ER);
theta3=sum(covmat_inv*EC);
theta4=EC'*covmat_inv*ER;
theta5=ER'*covmat_inv*ER;
theta6=EC'*covmat_inv*EC;
alpha_gamma=Gamma*theta2;
alpha_lambda=-Lambda*theta3;
mu=theta2/theta1+alpha_gamma*(theta5/theta2-theta2/theta1)+alpha_lambda*(theta4/theta3-theta2/theta1);
phi=theta3/theta1+alpha_gamma*(theta4/theta2-theta3/theta1)+alpha_lambda*(theta6/theta3-theta3/theta1);
v_sq=(theta5/theta2^2-1/theta1)*alpha_gamma.^2+(theta6/theta3^2-1/theta1)*alpha_lambda.^2+ ...
    (theta4/theta2/theta3-1/theta1)*alpha_gamma.*alpha_lambda + 1/theta1;
figure;
surf(mu,phi,v_sq);hold on
scatter3(ER,EC,v,'red','MarkerFaceColor',[1 1 1]);
xlabel('Expected returns')
ylabel('Centrality')
zlabel('Standard deviation')
saveas(gcf,"Possible solution.png")

%% simulation for 465 stocks in a quarter surface
rng default;
mu = zeros(465,1); Sigma = diag(ones(465,1));
X = mvnrnd(mu,Sigma,500);
covmat = cov(X);
cormat = corr(X);
v = diag(covmat);
n = length(covmat);
covmat_inv=diag(ones(1,n))/covmat;
[V,D]=eig(cormat-diag(ones(n,1)));
EC=V(:,1);EC=(EC-min(EC))/(max(EC)-min(EC));
ER=mean(X);ER=ER';

lambda=linspace(0,0.3,20);
gamma=linspace(0,0.3,20);
[Lambda,Gamma]=meshgrid(lambda,gamma);
theta1=sum(sum(covmat_inv));
theta2=sum(covmat_inv*ER);
theta3=sum(covmat_inv*EC);
theta4=EC'*covmat_inv*ER;
theta5=ER'*covmat_inv*ER;
theta6=EC'*covmat_inv*EC;
alpha_gamma=Gamma*theta2;
alpha_lambda=-Lambda*theta3;
mu=theta2/theta1+alpha_gamma*(theta5/theta2-theta2/theta1)+alpha_lambda*(theta4/theta3-theta2/theta1);
phi=theta3/theta1+alpha_gamma*(theta4/theta2-theta3/theta1)+alpha_lambda*(theta6/theta3-theta3/theta1);
v_sq=(theta5/theta2^2-1/theta1)*alpha_gamma.^2+(theta6/theta3^2-1/theta1)*alpha_lambda.^2+ ...
    (theta4/theta2/theta3-1/theta1)*alpha_gamma.*alpha_lambda + 1/theta1;
figure;
surf(mu,phi,v_sq);hold on
scatter3(ER,EC,v,'red','MarkerFaceColor',[1 1 1]);
xlabel('Expected returns')
ylabel('Centrality')
zlabel('Standard deviation')
saveas(gcf,"Efficient surface.png")


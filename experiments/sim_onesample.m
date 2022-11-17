function sim_onesample
    %%
    J = 36;                             % Number of simulations

    X = [0.0, 0.2, 0.4, 0.6, 0.8, 1.0]; % Realizable percent accuracy
    n = 155;                            % Number of participants

    M = 4096;                           % Number of CIs
    CI = zeros(M, 2);
    
    al = 0.05;                          % Nominal alpha-level
    t = tinv(1 - al/2, n - 1);

    %% Run simulations
    figure(1);
    clf; cla;

    for j=1:J
        p = drchrnd(ones(1, numel(X)), 1);  % Draw PMF form a uniform prior (Dirichlet distribution)
        mu = X*p';                          % True parameter

        %% Construct CIs
        for m=1:M
            I = mnrnd(n, p);
            mu_X = X*I'/n;
            sgm_X = sqrt(sum((X - mu).^2.*I)/(n - 1));
            C = t*sgm_X/sqrt(n);
            CI(m, :) = [mu_X - C, mu_X + C];
        end
        
        hitrate = mean(CI(:, 1) < mu & mu < CI(:, 2));

        %%
        subplot(6, 6, j);
        bar(X, p);
        xticks([0.0, 0.2, 0.4, 0.6, 0.8, 1.0]);
        xticklabels({'0.0', '0.2', '0.4', '0.6', '0.8', '1.0'});
        title([{['False positive rate: ', num2str((1 - hitrate)*100, '%3.3f'), '%']}, {'PMF'}], 'FontSize', 10);
        drawnow;
    end
end

function r = drchrnd(a, n)
    K = length(a);
    r = gamrnd(repmat(a, n, 1), 1, n, K);
    r = r./repmat(sum(r, 2), 1, K);
end
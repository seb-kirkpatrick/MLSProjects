
data {
  int<lower=1> N; # Matches
  int<lower=1> P; # Players
  #int<lower=1> T; # Teams
  #int<lower=1> S; # Seasons
  
  array[N] int<lower=1, upper=P> id;
  vector<lower=0>[N] xG;
  vector<lower=0>[N] xA;
  
  // Covariates
  vector<lower=0>[N] min_per_app;
  vector<lower=0>[N] player_xG;
  vector<lower=0>[N] player_xA;
  
  vector<lower=0>[N] last_g_min;
  #vector<lower=0>[N] last_g_xG;
  #vector<lower=0>[N] last_g_xA;
  
  #array[N] int<lower=1, upper=T> team_id;
  #array[N] int<lower=1, upper=T> opponent_id;
  #array[N] int<lower=1, upper=S> season_id;
}


parameters {
  real beta_xG;
  real beta_xA;
  cov_matrix[2] sigma_gc;
  
  vector[P] g;
  real<lower=0> sigma_g;
  vector[P] a;
  real<lower=0> sigma_a;
  
  real beta_min_xG;
  real beta_min_xA;
  real beta_player_xG;
  real beta_player_xA;
  
  real beta_last_g_min_xG;
  real beta_last_g_min_xA;
  #real beta_last_g_xG;
  #real beta_last_g_xA;
  
  #matrix[T, S] team_season_effects;
  #real<lower=0> sigma_team_season;
  #matrix[T, S] opp_season_effects;
  #real<lower=0> sigma_opp_season;
}

transformed parameters {
  matrix[N,2] mu;
  
  for (n in 1:N){
    mu[n,1] = beta_xG + g[id[n]]
              + beta_min_xG * min_per_app[n] 
              + beta_player_xG * player_xG[n] 
              + beta_last_g_min_xG * last_g_min[n] 
              #+ beta_last_g_xG * last_g_xG[n]
              #+ team_season_effects[team_id[n], season_id[n]]
              #+ opp_season_effects[opponent_id[n], season_id[n]]
              ;
    mu[n,2] = beta_xA + a[id[n]]
              + beta_min_xA * min_per_app[n] 
              + beta_player_xA * player_xA[n] 
              + beta_last_g_min_xA * last_g_min[n] 
              #+ beta_last_g_xA * last_g_xA[n]
              #+ team_season_effects[team_id[n], season_id[n]]
              #+ opp_season_effects[opponent_id[n], season_id[n]]
              ;
  }
}

model {
  
  for (n in 1:N){
    vector[2] xGC_n = [xG[n], xA[n]]';
    xGC_n ~ multi_normal(mu[n]', sigma_gc);
  }
  
  
  // Priors
  
  beta_xG ~ normal(0,10);
  g ~ normal(0, sigma_g);
  sigma_g ~ normal(0,10);
  
  beta_xA ~ normal(0,10);
  a ~ normal(0, sigma_a);
  sigma_a ~ normal(0,10);
  
  sigma_gc ~ inv_wishart(3, identity_matrix(2));
  
  beta_min_xG ~ normal(0, 10);
  beta_min_xA ~ normal(0, 10);
  beta_player_xG ~ normal(0, 10);
  beta_player_xA ~ normal(0, 10);
  
  beta_last_g_min_xG ~ normal(0, 10);
  beta_last_g_min_xA ~ normal(0, 10);
  #beta_last_g_xG ~ normal(0, 100);
  #beta_last_g_xA ~ normal(0, 100);
  
  #for (t in 1:T) {
  #  for (s in 1:S) {
  #    team_season_effects[t, s] ~ normal(0, 10);
  #    opp_season_effects[t, s] ~ normal(0, 10);
  #  }
  #}
  
  #sigma_team_season ~ normal(0, 10);
  #sigma_opp_season ~ normal(0, 10);
}



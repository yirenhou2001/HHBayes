// rsv_household_model_matching_simulator_fast.stan
data {
  int<lower=1> N;                
  int<lower=1> T;                
  int<lower=1> H;                
  int<lower=1> R;     
  
  real delta;

  int<lower=1, upper=H> hh_id[N];    
  int<lower=1, upper=R> role_id[N];  

  int<lower=0, upper=1> Y[N, T]; 
  int<lower=0, upper=1> I[N, T];
  real V[N, T];                         
  real V_term[N, T];                    // precomputed viral-load transformation

  real<lower=0> alpha_comm_by_role;       

  int<lower=0> max_infectious[N];    

  int<lower=1> hh_size[H];            
  int<lower=1> hh_max_size;           
  int<lower=1> hh_members[H, hh_max_size];  // padded household member indices

  vector[T] g_profile;                
  real<lower=0> V_ref; 
  
  // --- Fixed transmissibility parameters (from simulation) ---
  real<lower=0> beta1;    // baseline household transmissibility
  real<lower=0> beta2;    // viral-load mediated transmissibility coefficient
  
  //vector[R] kappa_by_role;
  
  real<lower=0> reference_phi;
  real<lower=0> reference_kappa;
  
  matrix[T, R] seasonal_forcing_mat;
  
  
}

transformed data {
  // first day of infectiousness for each individual (0 if never infectious)
  int first_inf[N];
  for (n in 1:N) {
    first_inf[n] = 0;
    for (t in 1:T) {
      if (Y[n, t] == 1) {
        first_inf[n] = t;
        break;
      }
    }
  }
}

parameters {

  vector<lower=0>[R-1] phi_by_role_raw; // this is a multiplier >0
  vector<lower=0>[R-1] kappa_by_role_raw; // this is a multiplier >0
 
}

transformed parameters {
  
  vector<lower=0>[R] phi_by_role; // this is real phi values
  phi_by_role[1] = reference_phi; 
  phi_by_role[2:R] = phi_by_role_raw * reference_phi;
  
  vector<lower=0>[R] kappa_by_role; // this is real phi values
  kappa_by_role[1] = reference_kappa; 
  kappa_by_role[2:R] = kappa_by_role_raw * reference_kappa;
  
}


model {
  // Priors
  
  phi_by_role_raw ~ normal(0, .5); 
  kappa_by_role_raw ~ normal(0, .5); 
  
  // Likelihood: discrete-time simulator
  for (n in 1:N) {
    for (t in 1:T) {
      if (t == 1 || sum(I[n, 1:(t-1)]) == 0) {  // only susceptible 
        real lambda = 0.0;
        
        // Community hazard
        lambda +=  phi_by_role[role_id[n]] * alpha_comm_by_role * seasonal_forcing_mat[t, role_id[n]];

        // Household contributions
        int h = hh_id[n]; // h gives the h-th households in the simulated dataset 
        real scaling_h = pow(1.0 / max(hh_size[h], 1), delta);

        for (m_idx in 1:hh_size[h]) { // 1:hh_size[h] loops over all household members in the household. 
          int m = hh_members[h, m_idx];
          if (m == n) continue;
          int f = first_inf[m];
          if (f == 0) continue;
          int tau = t - f;
          if (tau < 1 || tau > max_infectious[m]) continue;

          real gval = (tau <= T) ? g_profile[tau] : 0;
          real term1 = beta1 * gval;
          real term2 = beta2 * V_term[m, t];

         lambda += phi_by_role[role_id[n]] * kappa_by_role[role_id[m]] * scaling_h * (term1 + term2);
          
        }

        // Clamp lambda to avoid numerical issues
        lambda = fmin(fmax(lambda, 1e-12), 1e6);

        // Likelihood
        target += bernoulli_lpmf(I[n, t] | 1 - exp(-lambda));
      }
    }
  }
}

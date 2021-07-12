// see https://github.com/milkha/Splines_in_Stan/
functions {
  vector build_b_spline(real[] t, real[] ext_knots, int ind, int order);
  
  vector build_b_spline(real[] t, real[] ext_knots, int ind, int order) {
    // INPUTS:
    //    t:          the points at which the b_spline is calculated
    //    ext_knots:  the set of extended knots
    //    ind:        the index of the b_spline
    //    order:      the order of the b-spline
    vector[size(t)] b_spline;
    vector[size(t)] w1 = rep_vector(0, size(t));
    vector[size(t)] w2 = rep_vector(0, size(t));
    if (order==1)
    for (i in 1:size(t)) // B-splines of order 1 are piece-wise constant
    b_spline[i] = (ext_knots[ind] <= t[i]) && (t[i] < ext_knots[ind+1]); 
    else {
      if (ext_knots[ind] != ext_knots[ind+order-1])
      w1 = (to_vector(t) - rep_vector(ext_knots[ind], size(t))) / 
      (ext_knots[ind+order-1] - ext_knots[ind]);
      if (ext_knots[ind+1] != ext_knots[ind+order])
      w2 = 1 - (to_vector(t) - rep_vector(ext_knots[ind+1], size(t))) / 
      (ext_knots[ind+order] - ext_knots[ind+1]);
      // Calculating the B-spline recursively as linear interpolation of two lower-order splines 
      b_spline = w1 .* build_b_spline(t, ext_knots, ind, order-1) + 
      w2 .* build_b_spline(t, ext_knots, ind+1, order-1);
    }
    return b_spline;
  }
  
}


data {
  // number of surveys
  int<lower=0> N;      
  // outcome
  vector[N] A;  
  // estimates for covariate values
  vector[N] BANY;   
  vector[N] BEX;
  // other covariates known with certainty
  vector<lower=0, upper=1>[N] hdi; 
  vector<lower=1975, upper=2020>[N] SurveyYear;
  vector<lower=1, upper=7>[N] mean_bord; 
  // measurement noises
  vector<lower=0>[N] A_se;
  vector<lower=0>[N] BANY_se;
  vector<lower=0>[N] BEX_se;  
  // spline data
  int num_knots;            // num of knots
  vector[num_knots] knots;  // the sequence of knots
  int spline_degree;        // the degree of spline (= order - 1)
  
  // to check global spline function fitted
  int n_decades_for_pred;
  vector[n_decades_for_pred] decades_for_pred;
}


transformed data {
  vector[N] hdi_mod;
  vector[N] decade;
  
  // spline transformed data
  int num_basis = num_knots + spline_degree - 1; // total number of B-splines
  matrix[num_basis, N] B;  // matrix of B-splines
  vector[spline_degree + num_knots] ext_knots_temp;
  vector[2*spline_degree + num_knots] ext_knots; // set of extended knots
  
  hdi_mod = (hdi-0.2)*10;
  decade  = (SurveyYear-1975)/10;
  
  ext_knots_temp = append_row(rep_vector(knots[1], spline_degree), knots);
  ext_knots = append_row(ext_knots_temp, rep_vector(knots[num_knots], spline_degree));
  for (ind in 1:num_basis)
  B[ind,:] = to_row_vector(build_b_spline(to_array_1d(decade), to_array_1d(ext_knots), ind, spline_degree + 1));
}


parameters {
  // unknown true values
  vector[N] am;          
  vector[N] bany;    
  vector[N] bex;      
  // regression coefficients
  real c_intercept;          
  real c_decade;
  real c_bex;            
  real c_mean_bord;       
  real c_bany;         
  real c_hdi_mod;       
  real c_bany_hdi_mod; 
  // outcome noise
  real<lower=0> sigma_relationship; 
  // spline parameters
  row_vector[num_basis] a_raw; 
  real<lower=0> tau;   
}


transformed parameters {
  vector[N] cc_bany_hdi_mod;
  
  vector[N] decade_contribution;
  
  vector[N] predictor_normal;
  
  decade_contribution =  to_vector(a_raw*tau*B) + c_decade*decade;
  
  cc_bany_hdi_mod=c_bany_hdi_mod*hdi_mod;
  
  predictor_normal = c_intercept + c_bex*bex + decade_contribution +
  c_hdi_mod*hdi_mod + c_mean_bord*mean_bord + 
  c_bany*bany + cc_bany_hdi_mod .* bany ;
}


model {
  c_intercept ~ normal(0, 6);
  c_decade ~ normal(0, 2);
  c_bex  ~ normal(0, 2);
  c_mean_bord ~ normal(0, 2);
  c_hdi_mod ~ normal(0, 3);
  c_bany ~ normal(0.5, 0.5);
  c_bany_hdi_mod ~ normal(0, 0.5);
  
  sigma_relationship ~ cauchy(0, 5);
  
  
  bany ~ normal(20, 7.5);
  am ~ normal(13, 6);
  bex ~ normal(3, 2);
  
  // splines
  a_raw ~ normal(0, 3);
  tau ~ normal(0, 3);
  
  
  BANY ~ normal(bany, BANY_se);   
  BEX ~ normal(bex, BEX_se);
  
  am ~ normal(predictor_normal, sigma_relationship);
  
  A ~ normal(am, A_se);    
}


generated quantities{
  
  matrix[num_basis, n_decades_for_pred] B_pred;  
  vector[n_decades_for_pred] decade_effect; 
  
  for (ind in 1:num_basis)
  B_pred[ind,:] = to_row_vector(build_b_spline(to_array_1d(decades_for_pred), to_array_1d(ext_knots), ind, spline_degree + 1));

  decade_effect =  c_decade*decades_for_pred + to_vector(a_raw*tau*B_pred);
  
}






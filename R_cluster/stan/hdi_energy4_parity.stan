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
  // number of energy groups
  int<lower=0> N;      
  // outcome
  vector[N] A;  
  // estimates for covariate values 
  vector[N] BANY;   
  vector[N] BEX;
  // other covariates known with certainty
  vector<lower=0, upper=1>[N] urban; 
  vector<lower=0, upper=1>[N] elec;
  vector<lower=0, upper=1>[N] water;
  vector<lower=0, upper=1>[N] educ;
  vector<lower=0, upper=1>[N] par1;
  vector<lower=0, upper=1>[N] par4;
  vector<lower=1975, upper=2020>[N] SurveyYear;
  
  vector<lower=0, upper=1>[N] hdi; 
  
  // measurement noises
  vector<lower=0>[N] A_se;
  vector<lower=0>[N] BANY_se;
  vector<lower=-3>[N] BEX_se;  
  // spline data
  int num_knots;            // num of knots
  vector[num_knots] knots;  // the sequence of knots
  int spline_degree;        // the degree of spline (is equal to order - 1)
  
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
  real c_bany;       
  real c_hdi_mod;       
  real c_urban;    
  real c_elec;
  real c_water;
  real c_educ;
  real c_bex;            
  real c_bany_hdi_mod; 
  real c_bany_urban;
  real c_bany_elec;
  real c_bany_water;
  real c_bany_educ;
  real c_par1;    
  real c_par4;    
  real c_bany_par1;    
  real c_bany_par4;   
  // outcome noise
  real<lower=0> sigma_relationship; 
  // spline parameters
  row_vector[num_basis] a_raw; 
  real<lower=0> tau;   
}


transformed parameters {
  vector[N] cc_bany_hdi_mod;
  vector[N] cc_bany_urban;
  vector[N] cc_bany_elec;
  vector[N] cc_bany_water;
  vector[N] cc_bany_educ;
  vector[N] cc_bany_par1;
  vector[N] cc_bany_par4;
  
  vector[N] decade_contribution; 
  
  vector[N] predictor_normal;
  
  cc_bany_urban= c_bany_urban*urban;
  cc_bany_elec= c_bany_elec*elec;
  cc_bany_water= c_bany_water*water;
  cc_bany_educ= c_bany_educ*educ;
  
  cc_bany_par1 = c_bany_par1*par1;
  cc_bany_par4 = c_bany_par4*par4;
  
  decade_contribution = c_decade*decade + to_vector(a_raw*tau*B);
  
  cc_bany_hdi_mod=c_bany_hdi_mod*hdi_mod;
  
  predictor_normal = c_intercept + decade_contribution + c_bex*bex +
  c_urban*urban +  c_elec*elec + c_water*water + c_educ*educ +
  c_hdi_mod*hdi_mod +
  c_par1*par1 + c_par4*par4 + 
  c_bany*bany  + 
  (cc_bany_urban  + cc_bany_elec + cc_bany_water + cc_bany_educ) .* bany +
  cc_bany_hdi_mod .* bany +
  (cc_bany_par1   + cc_bany_par4) .* bany;
  
}


model {
  c_intercept ~ normal(0, 6);
  c_decade ~ normal(0, 2);
  c_bex  ~ normal(0, 2);
  
  c_hdi_mod ~ normal(0, 3);
  c_urban ~ normal(0, 3);
  c_elec ~ normal(0, 3);
  c_water ~ normal(0, 3);
  c_educ ~ normal(0, 3);
  c_par1 ~ normal(0, 3);
  c_par4 ~ normal(0, 3);
  c_bany ~ normal(0.5, 0.5);
  c_bany_hdi_mod ~ normal(0, 0.5);
  c_bany_urban ~ normal(0.5, 0.5);
  c_bany_elec ~ normal(0.5, 0.5);
  c_bany_water ~ normal(0.5, 0.5);
  c_bany_educ ~ normal(0.5, 0.5);  
  c_bany_par1 ~ normal(0.5, 0.5);     
  c_bany_par4 ~ normal(0.5, 0.5); 
  
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

generated quantities {
  
  real combined_narrow_intercept;
  real combined_narrow_slope;
  
  real combined_full_intercept;
  real combined_full_slope;
  
  
  real combined_full1hdi_intercept;
  real combined_full1hdi_slope;
  
  real combined_full2hdi_intercept;
  real combined_full2hdi_slope;
  
  real combined_full3hdi_intercept;
  real combined_full3hdi_slope;
  
  matrix[num_basis, n_decades_for_pred] B_pred;  
  vector[n_decades_for_pred] decade_effect; 
  
  combined_narrow_intercept =  c_elec + c_water + c_educ;
  combined_narrow_slope     =  c_bany_elec + c_bany_water + c_bany_educ;
  
  combined_full_intercept = combined_narrow_intercept + c_urban;
  combined_full_slope     = combined_narrow_slope     + c_bany_urban;
  
  combined_full1hdi_intercept = combined_full_intercept + 1*c_hdi_mod;
  combined_full1hdi_slope     = combined_full_slope + 1*c_bany_hdi_mod;
  
  combined_full2hdi_intercept = combined_full_intercept + 2*c_hdi_mod;
  combined_full2hdi_slope     = combined_full_slope + 2*c_bany_hdi_mod;
  
  combined_full3hdi_intercept = combined_full_intercept + 3*c_hdi_mod;
  combined_full3hdi_slope     = combined_full_slope + 3*c_bany_hdi_mod;
  
  for (ind in 1:num_basis)
  B_pred[ind,:] = to_row_vector(build_b_spline(to_array_1d(decades_for_pred), to_array_1d(ext_knots), ind, spline_degree + 1));
  
  decade_effect =  c_decade*decades_for_pred + to_vector(a_raw*tau*B_pred);
}




# Gaussian mixture model parameter spaces work

    Code
      print(normal_mixture_spaces, show_transformer = TRUE)
    Message
      
      -- Optimization Space <numeric(5)> ---------------------------------------------
    Output
                   
       [1:2] mu    
       [3:4] sd    
       [5:5] lambda
    Message
      
      -- Transformation to Interpretation Space: 
    Output
                                         
       mu     x                          
       sd     exp(x)                     
       lambda c(plogis(x), 1 - plogis(x))
    Message
      
      -- Interpretation Space <list(3)> ----------------------------------------------
    Output
                   
       [[1]] mu    
       [[2]] sd    
       [[3]] lambda
    Message
      
      -- Transformation to Optimization Space: 
    Output
                          
       mu     x           
       sd     log(x)      
       lambda qlogis(x[1])
      

# Probit parameter spaces work

    Code
      print(probit_parameter, TRUE)
    Message
      
      -- Optimization Space <numeric(12)> --------------------------------------------
    Output
                    
       [1:3]   b    
       [4:9]   Omega
       [10:12] Sigma
    Message
      
      -- Transformation to Interpretation Space: 
    Output
                                                   
       b     x                                     
       Omega oeli::chol_to_cov(x)                  
       Sigma oeli::undiff_cov(oeli::chol_to_cov(x))
    Message
      
      -- Interpretation Space <list(3)> ----------------------------------------------
    Output
                  
       [[1]] b    
       [[2]] Omega
       [[3]] Sigma
    Message
      
      -- Transformation to Optimization Space: 
    Output
                                                 
       b     as.vector(x)                        
       Omega oeli::cov_to_chol(x)                
       Sigma oeli::cov_to_chol(oeli::diff_cov(x))
      

# Parameter spaces with zero length parameter work

    Code
      print(zero_parameter_spaces, show_transformer = TRUE)
    Message
      
      -- Optimization Space <numeric(2)> ---------------------------------------------
    Output
               
       [1:1]  x
       [NULL] y
       [2:2]  z
    Message
      
      -- Transformation to Interpretation Space: 
    Output
                    
       x x          
       y c("A", "B")
       z x          
    Message
      
      -- Interpretation Space <list(3)> ----------------------------------------------
    Output
              
       [[1]] x
       [[2]] y
       [[3]] z
    Message
      
      -- Transformation to Optimization Space: 
    Output
                     
       x as.vector(x)
       y c()         
       z as.vector(x)
      


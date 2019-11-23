from __future__ import division
import numpy as np
import sys

X_train = np.genfromtxt(sys.argv[1], delimiter=",")
y_train = np.genfromtxt(sys.argv[2])
X_test = np.genfromtxt(sys.argv[3], delimiter=",")

def pluginClassifier(X_train, y_train, X_test):    
    x_train = np.array(X_train)
    y_train = np.array(y_train)
    X_test = np.array(X_test)
    
    emv = np.array(np.unique(y_train,return_counts=True))
    total=sum(emv[1])
    priors = emv[1]/total
    classes = emv[0]
           
    mean_class=[]  
    for i in range(len(classes)):
        mean_class.append(np.mean(x_train[y_train==classes[i]],axis=0))
    
    var_matrix=[]
    for i in range(len(classes)):
        var_matrix.append(np.cov(np.transpose(x_train[y_train==classes[i]])))

    fx=[]
    for j in range(len(X_test)):
        for i in range(len(classes)):
            a = np.linalg.det(var_matrix[i])
            mu = X_test[j] - mean_class[i]
            mu = np.array(mu.reshape(1,len(mu)))
            mu_t = np.transpose(mu)
            b = np.dot(mu,np.linalg.inv(var_matrix[i]))
            fx.append(priors[i]*(a**(-0.5))*np.exp(-0.5*np.dot(b,mu_t)))
            
    fx=np.array(fx)
    fx= fx.reshape(len(X_test),len(classes))
    
    fx_scaled=[]
    for k in range(len(fx)):
        fx_scaled.append(fx[k]/sum(fx[k]))
    return np.array(fx_scaled)
    pass

final_outputs = pluginClassifier(X_train, y_train, X_test) # assuming final_outputs is returned from function

np.savetxt("probs_test.csv", final_outputs, delimiter=",") # write output to file
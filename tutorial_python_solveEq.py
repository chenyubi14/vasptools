import numpy as np
# solve equation Ax=b, solution is given by r
A=np.array([[1,1,0],[1,0,2],[0,1,2]]) # equation coefficient matrix
b=np.array([2,3,3]).T # must be a column vector
r=np.linalg.solve(A,b) # solution


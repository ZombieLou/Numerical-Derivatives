# What this Program Does

 The program allows the user to approximate the second derivative at a point of their choosing 
 of the function x\*sin(x)=f(x). The function will calculate the second derivative exactly and then approximate it using euler's 3 and 5 points methods. Euler's three point method takes one point on the curve on each side of the point choosen, while Euler's five point method takes two points on the curve on either side of the point choosen. The program will then compare these two methods to the exact analytical method. There is a `.ipynb` file that will be provided and will provide the plots of the three second derivative values for various point separation, the errors of the two approximation methods, and a representation of the slopes of the errors of the two approximations. 

# How to Use

To use the program, make sure all `.f90` files are in the same directory and type "make" into the terminal, this will compile the files and create an executable called `derivatives`. In the Terminal type "./derivatives" to run the program. You will be         prompted to enter an x value around which to calculate the second derivative, enter a real number and hit "Enter". It will tell you the derivatives were written into a file called `results.dat`. Open this file if you wish to see the raw data of the calculations. Otherwise you can visualize them by running the jupyter notebook `.ipynb` file. 

let rec Bisection_Method (f:float -> float) (a:float) (b:float) (tol:float) (iternum:int) :float =
    //assert that there is a root inside the interval
    if (sign(f a) * sign(f b) > 0) then 
        printfn "No root inside the interval! Returning 0."
        0.0
    else
        let x_new = (a+b)/2.0
        //if our approximation is not good enough, recursively iterate until it is.
        if (b-a >= tol ) then
            printfn "iternum: %d | x_new: %f" iternum x_new
            (*if the sign of the function at the midpoint is equal to the sign of the function at our current point,
            discard the lower half of the interval 
            *)
            if sign(f a) * sign(f x_new) > 0 then
                Bisection_Method f x_new b tol (iternum+1)
            else
                Bisection_Method f a x_new tol (iternum+1)
        else
            printfn "\nfinal number of iterations: %d | root value: %f" iternum x_new
            x_new

//sample function: 2x^2 - 11x + 5
let sample_function x:float = 2.0*x**2.0 - 11.0*x + 5.0
Bisection_Method sample_function 2.0 7.0 0.000001 0

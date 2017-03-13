fun factorial(n) = (
                                        if (n<1)
                                        then (return 1;)
                                        else (return n*factorial(n-1););
                                )

                                     fun main() = (
                                          i := 1;
                                          while i<11 do (
                                          (print(factorial(i));)
                                          i := i+1;
                                          )
                                )
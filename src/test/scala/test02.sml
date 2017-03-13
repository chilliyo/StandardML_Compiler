fun factorial(n) = (
                                        if (n<1)
                                        then (return 1;)
                                        else (return n*factorial(n-1););
                                )

                                     fun main() = (
                                          i := 1;
                                          while i<10 do (
                                          (print(i);)
                                          i := i+1;
                                          )
                                )
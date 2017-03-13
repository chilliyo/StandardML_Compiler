fun mod(n) = (
                                        if (n%3=0)
                                        then (return n;)
                                        else (return 0;);
                                )

                                     fun main() = (
                                          i := 1;
                                          while i<101 do (
                                          (print(mod(i));)
                                          i := i+1;
                                          )
                                )
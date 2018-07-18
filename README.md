# glim

Inspired by DTrace output, glim is an experiment to ingest series of
numbers from standard in and display the output distribution.

It accepts a list of numbers seperated by new lines from stdin.

For example, to see the distribution of length of each find in the current directory

```
$ find . -type f |xargs wc -l |awk '{print $1}' |glim

   value  ------------- Distribution ------------- count
       1 |@@@@@@@@@@                               27
       2 |@@@@@                                    13
       4 |@@                                       5
       8 |@                                        4
      16 |@@@@@                                    14
      32 |@@@@@@@                                  18
      64 |@@                                       5
     128 |@@@                                      7
     256 |@                                        3
     512 |@@                                       5
    1024 |                                         1
    2048 |@@                                       5
    4096 |                                         0
    8192 |                                         0
   16384 |                                         1
          ----------------------------------------
 ignored                                           0
   total                                           108
```

Numbers are counted for given value if they are greater than or equal to the
given value, and less than the next value.

## Build

```
stack build
```

## Test

```
stack test
```

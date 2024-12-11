# Day 11

The naive algorithm of recursively counting the the stones it splits into performs poorly and does not even terminate in a reasonable amount of time for the second part. One can see that the amount of different stone types stabilizes at 3811, so it is much more efficient to save for every stone type the number of stones with this type.

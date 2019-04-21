echo 'Arguments:' $1 $2 $3

bla=34

echo 'bla:' $bla

read i1
read i2

echo 'Read two lines:' "$i1 and $i2"

read n m

echo -n Read a line with a word $n
echo " and" rest of line: $m

cd ..
pwd

exit 5
echo "not reachable"
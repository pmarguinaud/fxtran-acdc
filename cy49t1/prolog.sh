
mkdir -p /tmp/$USER

export PATH=/home/gmap/mrpm/marguina/fxtran-acdc/cy49t1:$PATH
export PATH=/home/gmap/mrpm/marguina/fxtran-acdc/bin:$PATH

function resolve ()
{
  f=$1
  u=$2

  g=""

  for view in $(cat .gmkview)
  do  
    g="src/$view/$f"
    if [ -f $g ]
    then
      break
    fi  
  done

  if [ "x$g" != "x" -a "x$u" = "x1" ]
  then
    if [ $(id -u) -ne $(stat --format='%u' $g) ]
    then
      cp $g "src/local/$f"
      g=src/local/$f
    fi
  fi

  echo $g
}



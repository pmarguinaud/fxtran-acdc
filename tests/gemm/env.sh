host=$(hostname)

if [ $host = "mi300x" ]
then
  source ./env.muncl01.sh
fi

if [[ $host =~ (taranis|belenos) ]] 
then 
  source ./env.meteo.sh
fi

if [[ $host =~ a[a-z][0-9]-[0-9]+\.bullx ]]
then
  source ./env.ecmwf.sh
fi


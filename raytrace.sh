if [ -z $1 ]
then
    echo "please specify scene"
else
    dist/build/raytrace/raytrace $1 +RTS -s -N8 && eog out.png
fi


try() {
    out=`echo -n "$1" | tr ^ '\012' | $2 | tr '\012' ^`
    if [ "$3" = "$out" ]; then
         echo "ok"
    else
        echo "$out != $3"
    fi            
}
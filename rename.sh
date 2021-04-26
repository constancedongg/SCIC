for f in *.mc; do
    mv -- "$f" "${f%.mc}.sc"
done
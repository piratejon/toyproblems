sed -e 's/[^-0-9]\+/\n/g' < input | awk 'BEGIN{c=0}{c+=}END{print c}'

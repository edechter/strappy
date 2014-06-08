 #!/bin/sh
  head -`fgrep -n END_SAMPLE Run.hp | tail -1 | cut -d : -f 1` Run.hp \
    | hp2ps > FOO.ps
  gv Run.ps &
  gvpsnum=$!
  while [ 1 ] ; do
    sleep 10
    head -`fgrep -n END_SAMPLE Run.hp | tail -1 | cut -d : -f 1` Run.hp \
      | hp2ps > Run.ps
    kill -HUP $gvpsnum
  done
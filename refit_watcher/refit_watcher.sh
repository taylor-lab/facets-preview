#!/bin/sh

WATCHER_DIR="/juno/work/ccs/shared/software/refit_watcher/"

INOTIFY_SH="$WATCHER_DIR/bin/refit_watcher_inotify_cmd.sh"

nohup sh -c "sleep 4; touch $WATCHER_DIR/refit_jobs/facets_refit_cmd_watcher_poll.sh" 2> /dev/null > /dev/null &

echo "
$WATCHER_DIR/bin/inotifywait --format '%w%f' --include 'facets_refit_cmd_.*sh\$' -m -e close_write $WATCHER_DIR/refit_jobs/ | \
  while read FILE
  do
    CUR_TIME=\`date +\"%Y-%m-%d %T\"\`
    if [[ \"\$FILE\" =~ \"facets_refit_cmd_watcher_poll.sh\" ]]; then
        echo \$CUR_TIME inotify is running
        nohup bash -c 'sleep 59; touch $WATCHER_DIR/refit_jobs/facets_refit_cmd_watcher_poll.sh' &
    elif [[ \"\$FILE\" =~ \"bsub.sh\" ]]; then
        echo \$CUR_TIME Running: \$FILE
        bsub -We 1:59 -n 4 -R \"rusage[mem=16]\" -o \$FILE.log -e \$FILE.err bash \$FILE
        echo \$CUR_TIME Finished: \$FILE
    else
        echo \$CUR_TIME Running: \$FILE
        bash \$FILE > \$FILE.log 2> \$FILE.err &
        echo \$CUR_TIME Finished: \$FILE
    fi
done " > $INOTIFY_SH

sleep 1

nohup bash $INOTIFY_SH >> $WATCHER_DIR/watcher.log 2>> $WATCHER_DIR/watcher.err &

sleep 4

rm -f $INOTIFY_SH

#!/bin/sh


WATCHER_DIR="/dmp/hot/ptashkir/lymphoma/copy_number/refit_watcher"


INOTIFY_SH="$WATCHER_DIR/bin/refit_watcher_inotify_cmd.sh"

rm -f $INOTIFY_SH

nohup sh -c 'sleep 4; touch $WATCHER_DIR/refit_jobs/facets_refit_cmd_watcher_poll.sh' 2>/dev/null 1>/dev/null &

echo "
$WATCHER_DIR/bin/inotifywait --format '%w%f' $WATCHER_DIR/refit_jobs -m -e close_write |grep 'facets_cmd_*sh$' --line-buffered | \
  while read FILE
  do
    CUR_TIME=\`date +\"%Y-%m-%d %T\"\`
    if [[ \"\$FILE\" =~ \"facets_refit_cmd_watcher_poll.sh\" ]]; then
        echo \$CUR_TIME inotify is running
        nohup bash -c 'sleep 59; touch \$WATCHER_DIR/refit_jobs/facets_refit_cmd_watcher_poll.sh' &
    else
        echo \$CUR_TIME Running: \$FILE
        bash \$FILE 1>>$WATCHER_DIR/run_logs/stdout 2>>$WATCHER_DIR/run_logs/stderr &
        echo \$CUR_TIME Finished: \$FILE
    fi
done " > $INOTIFY_SH

sleep 1

nohup bash $INOTIFY_SH >> $WATCHER_DIR/watcher.log 2>> $WATCHER_DIR/watcher.err &

sleep 4

rm -f $INOTIFY_SH

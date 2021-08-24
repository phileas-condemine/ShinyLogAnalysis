/usr/bin/Rscript /home/drees/shinyapps_logs/generate_daily_graph.R
cd /home/drees/render_nb_visits_shinyapps
git add .
git commit -m 'daily update'
git push github master

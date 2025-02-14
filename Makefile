.PHONY:
run_app :
	cd shiny && ./run_app.R
prepare_data :
	code/prepare_data.R
shiny_data :
	rm shiny/data/*.csv && cp data/*.csv shiny/data/


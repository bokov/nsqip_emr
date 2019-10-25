ts_explore: ts_explore.R
	export R_RENDERFILE="$<"; Rscript buildstuff.R

clean:
	rm -rf *.html *.md *.docx *.R.rdata *_files/ *_cache/

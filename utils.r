# https://stat.ethz.ch/pipermail/r-help/2009-January/183960.html
ggsave.latex <- function(..., caption = NULL, label = NULL, figure.placement = "hbt", floating = TRUE, caption.placement="bottom", latex.environments="center"){
	ggsave(...)
	
	cat("\n\n")
	if(floating){
		cat("\\begin{figure}[", figure.placement, "]\n", sep = "")
	}
	cat("    \\begin{", latex.environments,"}\n", sep = "")
	if(!is.null(caption) && caption.placement == "top"){
		cat("        \\caption{", caption, "}\n", sep = "")
	}
	args <- list(...)
	if(is.null(args[["filename"]])){
		if(is.null(args[["path"]])){
			args[["path"]] <- ""
		}
		if(is.null(args[["plot"]])){
			names(args)[which(names(args) == "")[1]] <- "plot"
		}
		args[["filename"]] <- paste(args[["path"]], digest.ggplot(args[["plot"]]), ".pdf", sep="")
	}
	
	if(is.null(args[["width"]])){
		if(is.null(args[["height"]])){
			cat("        \\includegraphics[height = 7in, width = 7in]{", args[["filename"]], "}\n", sep = "")
		} else {
			cat("        \\includegraphics[height = ", args[["height"]], "in, width = 7in]{", args[["filename"]], "}\n", sep = "")
		}
	} else {
		if(is.null(args[["height"]])){
			cat("        \\includegraphics[height = 7in, width = ", args[["width"]], "in]{", args[["filename"]], "}\n", sep = "")
		} else {
			cat("        \\includegraphics[height = ", args[["height"]], "in, width = ", args[["width"]], "in]{", args[["filename"]], "}\n", sep = "")
		}
	}
	if(!is.null(caption) && caption.placement == "bottom"){
		cat("        \\caption{", caption, "}\n", sep = "")
	}
	if(!is.null(label)){
		cat("        \\label{", label, "}\n", sep = "")
	}
	cat("    \\end{", latex.environments,"}\n", sep = "")
	if(floating){
		cat("\\end{figure}\n")
	}
	cat("\n\n")
}

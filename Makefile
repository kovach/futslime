slime1.py: slime1.fut
	futhark pyopencl --library slime1.fut

slime-out.gif: slime1.py run.py
	python run.py

slime-out.webm: slime-out.gif
	convert slime-out.gif -coalesce -limit memory 2gb -fuzz 5% slime-out.webm
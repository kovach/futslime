slime1.py: slime1.fut
	futhark pyopencl --library slime1.fut

slime2.py: slime2.fut
	futhark pyopencl --library slime2.fut

slime3.py: slime3.fut
	futhark pyopencl --library slime3.fut

slime-out.mp4: slime1.py slime2.py slime3.py run.py
	python run.py

slime-out.webm: slime-out.gif
	convert slime-out.gif -coalesce -fuzz 5% slime-out.webm
package cdmuhlb.dgview.io

import java.io.{File, PrintWriter}

case class Html5Video(framePrefix: String, width: Int, height: Int, fps: Int) {
  def writeConversionScript(scriptFile: File, movieName: String): Unit = {
    val out = new PrintWriter(scriptFile)
    out.println(raw"""
#!/bin/bash

ffmpeg -i '$framePrefix%04d.png' -pix_fmt yuv420p -f yuv4mpegpipe '$movieName.yuv'

x264 --crf 20 --preset veryslow --tune animation --keyint $fps --fps $fps \
     --profile main --level 3.1 --vbv-maxrate 14000 --vbv-bufsize 14000 \
     --non-deterministic --output '$movieName.mp4' --demuxer y4m '$movieName.yuv'
MP4Box -inter 500 '$movieName.mp4'

vpxenc '$movieName.yuv' -p 2 --best --target-bitrate=2000 --end-usage=vbr \
       --auto-alt-ref=1 --fps=$fps/1 -v --minsection-pct=5 --maxsection-pct=800 \
       --lag-in-frames=16 --kf-min-dist=0 --kf-max-dist=$fps --token-parts=2 \
       --static-thresh=0 --drop-frame=0 --min-q=0 --max-q=60 -o '$movieName.webm'

rm '$movieName.yuv'

cat << "EOF"
<video width="$width" height="$height" controls="controls" loop="loop">
  <source src="$movieName.mp4" type="video/mp4; codecs=avc1.4D401F" />
  <source src="$movieName.webm" type="video/webm; codecs=vp8" />
  <a href="$movieName.mp4">$movieName.mp4</a>
</video>
EOF
    """.trim)
    out.close()
  }
}

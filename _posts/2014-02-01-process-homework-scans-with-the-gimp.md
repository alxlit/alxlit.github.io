---
title: Process homework scans using the GIMP
layout: post
---

I attend [Virginia Tech][1], where it is *strongly recommended* that engineering students purchase a tablet PC (e.g. [Fujitsu's Lifebook][2]) from the bookstore, around $2000 with a four year warranty.
The idea I suppose is to save trees, enable note-taking directly on lecture slides, interactive classrooms via [DyKnow][3], yadda yadda.

The cheaper, more sensible option is to buy a mid-range laptop, a printer/scanner combo, and a stack of 50&cent; one-subject notebooks.
My laptop was $800 from Best Buy and came with an HP Photosmart.
Haven't had a single problem with it.
It was a little awkward for the first semester, being the only one without a tablet, but after that nobody cares.

I think only three or four of my classes required handwritten assignments to be turned in online.
For these classes I did my assignments in pencil, scanned them with [Simple Scan][4] (takes 5 minutes via USB, a lot longer over wireless), use the [GIMP][5] to clean up the lines and crop to a reasonable size, and ImageMagick's [`convert`][6] utility to put the images into an optimized PDF.

The GIMP has a wonderful [Guile][7] API, which you can browse by going to *Help* > *Procedure Browser*, and use by putting `.scm` files into `~/.gimp-2.x/scripts`.

Here's my `homework-scans.scm`:

{% highlight scheme %}
(define (homework-scans filename)
  (let* ((image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
         (drawable (car (gimp-image-get-active-layer image))))
    (gimp-image-crop image 2300 3300 0 0)
    (gimp-levels drawable HISTOGRAM-VALUE 75 210 1.0 0 255)
    (gimp-file-save RUN-NONINTERACTIVE image drawable filename filename)
    (gimp-image-delete image)))
{% endhighlight %}

Here's the accompanying shell script, `gimp-homework-scans`:

{% highlight sh %}
#!/bin/sh

FILES=*.jpg;

for f in $FILES; do
  gimp -i -b "(homework-scans \"$f\")" -b "(gimp-quit 0)"
done

convert $FILES -density 150 -page letter homework.pdf
{% endhighlight %}

Note that it's loading up the GIMP (without the GUI) per scan.
You could probably avoid this by putting all the `-b` options together, or figure out how to do it through the API.

[1]: http://vt.edu
[2]: http://www.shopfujitsu.com/store/
[3]: http://www.dyknow.com/
[4]: https://launchpad.net/simple-scan
[5]: http://gimp.org
[6]: http://imagemagick.sourceforge.net/http/www/convert.html
[7]: https://www.gnu.org/software/guile/
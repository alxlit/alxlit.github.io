---
title: Cleaning handwriting scans using the GIMP
layout: post
modified_date: 26 May 2015
---

TODO insert example.

## Motivation

[Virginia Tech][1] used to require its engineering students to purchase a hybrid laptop.
The idea, I suppose, was to save trees, enable note taking directly on lecture slides, interactive classrooms, etc.
But they really expensive and the loaded with buggy software, and I wasn't into it.

The cheaper, better option was to buy a mid-range laptop, a printer/scanner combo, a stack of 50&cent; one-subject notebooks, and a good pen.
My laptop came with an HP Photosmart that plays well with CUPS. 
It was a little awkward for the first semester (being the only one without a tablet) but after that nobody cared.

The problem was that a few of my classes (maybe two or three) required handwritten assignments to be turned in online.
For these classes I did my assignments in pencil and scanned them using [Simple Scan][4].
But the raw scans were ugly and unwieldy, often several megabytes in size.

## Scripting up a solution

After scanning, I used the [GIMP][5] to clean up the lines and crop to a reasonable size, and used ImageMagick's [`convert`][6] utility to put the images into an optimized PDF.
The GIMP has an excellent [Guile][7] API, which you can explore by going to *Help* > *Procedure Browser*.
You use it by putting `.scm` files into `~/.gimp-2.x/scripts`.

Here's my `homework-scan.scm`:

{% highlight scheme %}
(define (homework-scan filename)
  (let* ((image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
         (drawable (car (gimp-image-get-active-layer image))))
    (gimp-image-crop image 2300 3300 0 0)
    (gimp-levels drawable HISTOGRAM-VALUE 75 210 1.0 0 255)
    (gimp-file-save RUN-NONINTERACTIVE image drawable filename filename)
    (gimp-image-delete image)))
{% endhighlight %}

Here's the accompanying shell script, `prepare-homework-scans`:

{% highlight sh %}
#!/bin/sh

FILES=*.jpg;

for f in $FILES; do
  gimp -i -b "(homework-scan \"$f\")" -b "(gimp-quit 0)"
done

convert $FILES -density 150 -page letter homework.pdf
{% endhighlight %}

Note that it's loading up the GIMP (headless) per scan.
You could probably avoid this by putting all the `-b` options together, or figure out how to do it purely through the API.

[1]: http://vt.edu
[4]: https://launchpad.net/simple-scan
[5]: http://gimp.org
[6]: http://imagemagick.sourceforge.net/http/www/convert.html
[7]: https://www.gnu.org/software/guile/

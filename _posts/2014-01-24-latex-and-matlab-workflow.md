---
title: LaTeX and MATLAB workflow
layout: post
modified_date: 26 May 2015
tags: [school, tex]
---

I used MATLAB and LaTeX extensively last semester.
Each project (eight or nine, plus other assignments) was a chance to revise my workflow.
By the end of the semester I had come up with a simple, efficient workflow that produced high-quality results.

Here's an [example](/assets/latex-matlab-example.pdf) document and [source code](/assets/latex-matlab-example.zip).

1. TOC
{:toc}

## Preface

[LaTeX][0] is a document preparation system.
It's not used much outside of academia, which is a shame because there are a lot of great things about it:

* It's free (gratis and libre) and cross platform
* Really [nice][1] [typesetting][2]
* Separation of content and presentation
* Source files are text
* Lots of powerful libraries

Programs like Word struggle with technical papers that have lots of figures and equations.
Editing just a dozen or so pages of such content can be painfully slow.
This isn't a problem at all with LaTeX since it's just plaintext.

The usual argument against LaTeX is that it is difficult to install and use.
Perhaps this was true in the past, but today all you have to do is grab the latest [TeX Live][3] distribution and consult the [Wikibook][4].

<aside>
<p>Recent versions of <a href="http://gnu.org/software/octave">Octave</a> support PGF/TikZ out of the box, using the <a href="https://www.gnu.org/software/octave/doc/interpreter/Graphics-Toolkits.html">gnuplot</a> toolkit.</p>
</aside>

[MATLAB][5] is a numerical computing environment, pervasive in engineering.
Its simple language, comprehensive libraries, documentation, and IDE are hard to beat.
It has extensive plotting capabilities, but does not currently support exporting to [PGF/TikZ][7].
Forunately, there's an excellent library called [matlab2tikz][8] that does the job.

## The workflow

### Structure

Here's how I would typically structure a project:

<pre><code>diary
└── part_1
m
├── helpers
│   ├── diary_on.m
│   ├── diary_off.m
│   ├── resample_for_tikz.m
│   ├── save_tikz.m
│   ├── strucdisp.m
│   └── thing.m
├── matlab2tikz
│   ├── cleanfigure.m
│   ├── figure2dot.m
│   ├── matlab2tikzInputParser.m
│   ├── matlab2tikz.m
│   └── updater.m
└── parts
    ├── part_1.m
    └── ...
tex
├── plots
│   ├── part_1_mag_db.tex
│   └── ...
└── project.tex
</code></pre>

The `m/` directory (and its subdirectories) are added to MATLAB's path.

### Helpers

My `save_tikz` function is just a wrapper around `matlab2tikz` that writes to `tex/plots/`, disables auto-updates, and has some other options depending on the project:

{% highlight matlab %}
function save_tikz(filename, markersize)
lh = findall(gcf, 'type', 'line');
if nargin < 2
    markersize = 6.25;
end
set(lh, 'MarkerSize', markersize);
filename = sprintf('./tex/plots/%s.tex', filename);
matlab2tikz(filename, 'height', '\figheight', 'width', '\figwidth', ...
    'showInfo', false, 'checkForUpdates', false, ...
    'extraTikzpictureOptions', 'matlab2tikz picture', ...
    'parseStrings', false);
{% endhighlight %}

The `diary_on` function simply puts the diary files in `diary/`:

{% highlight matlab %}
function diary_on(filename)
f = sprintf('./diary/%s', filename);
delete(f)
diary(f);
{% endhighlight %}

The `diary_off` function is really just a stub, perhaps I'll use it in the future&hellip;

{% highlight matlab %}
function diary_off()
diary off;
{% endhighlight %}

<aside>
<p>
You could use <code>disp</code> or <code>x = x</code>, but these do not print structure contents.
</p>
</aside>

The `thing` function just prints out the contents of a variable, using [`strucdisp`][9] if it's a structure.

{% highlight matlab %}
function thing(name, thing)
fprintf('%s = \n', name);
if isstruct(thing)
   strucdisp(thing);
else
   disp(thing);
end
{% endhighlight %}

The `resample_for_tikz` function is explained a little bit later.

### Working in parts

Here's a laundry list of suggestions for writing MATLAB code.

* **Don't do all your work in a single script.** And try to write functions.
* **Keep scripts/functions small.** Say, less than 200 lines.
* **Don't use workspace variables to pass along information.**
  Relying on global variables makes dependency tracking (what variables must I provide in order to use this?) very difficult.
* **Don't blindly `load` data.** The explicit form `load datafile x y z` is more readble, can catch errors, and doesn't risk polluting your scope.
* **Try to section off I/O.** Stuff like plotting, writing data files, etc. are really time-consuming.

So what's a better way?
Try the following:

{% highlight matlab %}
function [za, pa, ka] = part_1()
%%
enable_plots = ~nargout || nargout == 16;

% some design process...

za = [];
pa = [-0.31927 + 1.20318i, -0.31927 + 1.20318i, -0.77078 + 0.49837i, \
      -0.77078 - 0.49837i];
ka = 1;

if enable_plots;
    diary_on('part_1');
  
    thing('za', za);
    thing('pa', pa);
    thing('ka', ka);
 
    % look at the frequency response
    H = zpk(za, pa, ka);
    n = 1024;
    w = linspace(0, 2*pi, n);
    Hfr = freqresp(H, w);
    Hfr = Hfr(:);

    figure;
    plot(w, db(Hfr), 'r-');
    xlim([0 2*pi]);
    xlabel('Frequency (rad/s)');
    ylabel('Magnitude (dB)');
    save_tikz('part_1_mag_db');

    diary_off();
end
{% endhighlight %}

On the first line the outputs are explicitly specified.
You could also specify inputs (and provide defaults the usual way, by checking `nargin`).

On the second line I set a code fold using `%%`.
I can run `part_1` as a script and have the variables dumped to the workspace using `C-Enter` (just remember to `clear all` when you're done).
If I want to use the debugger, I can set a few breakpoints and run it as a function by hitting `F5`.
You get the best of both worlds.

On the third line I check whether `part_1` is the main thing being run, i.e. that it's not being called from another script or function, in which case I probably want to look at graphs and such.

### Using LaTeX

Here's a basic preamble for working with PGF/TikZ.

{% highlight tex %}
\usepackage{graphicx}
\usepackage{pgfplots}
\usepackage{standalone}
\usepackage{tikz}

\usetikzlibrary{external}
\tikzset{external/system call={lualatex \tikzexternalcheckshellescape -halt-on-error -interaction=batchmode -jobname "\image" "\texsource"}}
\tikzexternalize

\pgfplotsset{
  every axis legend/.append style={font=\footnotesize},
  tick label style={font=\footnotesize},
  ylabel style={yshift=-0.5em},
  yticklabel style={/pgf/number format/fixed},
  xticklabel style={/pgf/number format/fixed},
  /tikz/matlab2tikz picture/.append style={trim axis left, trim axis right}
}

\newlength\figheight
\newlength\figwidth
{% endhighlight %}

To include a plot:

{% highlight tex %}
\begin{figure}[!ht]
  \centering
  \setlength\figheight{3in}
  \setlength\figwidth{3in}
  \include{./plots/part_1_mag_db.tex}
  \caption{Frequency response (magnitude)}
  \label{fig:part_1_mag_db}
\end{figure}
{% endhighlight %}

The `external` library renders plots to separate numbered PDF files, e.g. `project_figure-0.pdf`.
Plots are re-rendered when their source file is modified.
Just note that you have to configure your editor to use `-shell-escape` (see below).

I use `lualatex`, which is included with TeX Live.
The default `pdflatex` runs out of memory quickly when rendering TikZ (uses a fixed amount that is hard to configure).
I haven't run into any compatability issues yet.

As a rule of thumb, you should try to limit your generated TikZ plots to less than 250 KiB, to keep render time short.
This can be a problem for plots with lots of data, and where uniform decimation doesn't preserve the original shape.
You may have to get creative.

Here's an example `resample_for_tikz` I use for energy spectral densities (very ad hoc):

{% highlight matlab %}
function indices = resample_for_tikz(x, n)

m = length(x);
indices = zeros(1,n);
num_blocks = 100;
energy = abs(x).^2;
block_size = ceil(m / num_blocks);
blocks = min(block_size * (0:num_blocks), m);
block_energy = sum(energy) / num_blocks;
samples_per_block = floor(n / num_blocks);

j = 1;
for k = 2:num_blocks+1;
    a = blocks(k-1)+1;
    b = blocks(k);
    
    % sort points by energy
    [~, ix] = sort(energy(a:b) / block_energy);

    p = length(ix);

    % sample logarithmically (err, sort of, kind of, well not really)
    l = linspace(1, p, samples_per_block);
    l = floor(p*log(l)/log(p));
    l(1) = 1;

    indices(j:j+samples_per_block-1) = a + sort(ix(l));

    j = j+m;
end;

% it's not perfect...
indices = indices(indices ~= 0);
{% endhighlight %}

Example usage:

{% highlight matlab %}
x = get_signal_from_somewhere();
n = length(x);
fs = 1e3;
nfft = 2^nextpow2(n);
fmax = n*fs;
f = linspace(-fmax/2, fmax/2, nfft);
esd = fftshift(fft(x, nfft));
ix = resample_for_tikz(esd, 2e3);

figure;
plot(f(ix)/1e3, db(esd(ix)));
xlabel('Frequency (kHz)');
ylabel('Magnitude (dB)');
save_tikz('Energy spectral density of $x(t)$');
{% endhighlight %}

### Configuring your editor

#### Emacs

There are a few options for Emacs (with AUCtex).
If you don't care about the external stuff and just want to use `lualatex`, add `(setq TeX-engine 'lualatex)` to your your `init.el`.
I like to use `latexmk` to about having to `C-c C-c` multiple times, but it's not supported out of the box.
Have a look [here][10] or [here][11] on how to set it up yourself.

#### Vim

I got fed up trying to use Vim-LaTeX, and ended up just using `:make`.

{% highlight vim %}
au FileType * setlocal mp=latexmk\ -pdf\ -e\ '$pdflatex=\"lualatex\ -file-line-error\ -shell-escape\ -synctex=1\ \\%S\ \\%O\"'\ -f\ '%'
{% endhighlight %}

## Closing thoughts

Hopefully this article gives you some idea of how MATLAB and LaTeX can work together.

[0]: http://wikipedia.org/wiki/LaTeX
[1]: http://nitens.org/taraborelli/latex
[2]: http://tex.stackexchange.com/questions/110133/visual-comparison-between-latex-and-word-output-hyphenation-typesetting-ligat
[3]: http://www.tug.org/texlive
[4]: http://wikibooks.org/wiki/LaTeX
[5]: http://mathworks.com
[7]: http://wikipedia.org/wiki/PGF/TikZ
[8]: https://github.com/matlab2tikz/matlab2tikz
[9]: http://www.mathworks.com/matlabcentral/fileexchange/13500-structure-outline/content/strucdisp.m
[10]: http://tex.stackexchange.com/questions/124793/emacs-and-latexmk-setup-for-shell-escape
[11]: https://github.com/tom-tan/auctex-latexmk


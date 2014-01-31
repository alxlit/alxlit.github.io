---
title: LaTeX and MATLAB workflow
layout: post
tags: [school, tex]
---

I used MATLAB and LaTeX extensively last semester.
Each project (like, eight or nine, plus other assignments) was a chance to revise my workflow.
By the end of the semester I had come up with a simple, efficient workflow that produced high-quality results.

## Preface

[LaTeX][0] is a document preparation system. 
It isn't used much outside of academia, which is a shame because it really is superior to the likes of Word in a number of ways:

* It's free (both *gratis* and *libre*), cross platform
* Much [better][1] [typesetting][2]
* Separation of content and presentation
* Source files are text
* Lots of powerful libraries

The usual argument against LaTeX asserts that it is difficult to install and use.
Perhaps this was true in the past, but these days all you have to do is grab the latest [TeX Live][3] distribution.
There's a learning curve, to be sure, but have no fear: The [Wikibook][4] will hold your hand.

<aside>
<p>Recent versions of <a href="http://gnu.org/software/octave">Octave</a> support PGF/TikZ out of the box, using the <a href="https://www.gnu.org/software/octave/doc/interpreter/Graphics-Toolkits.html">gnuplot</a> toolkit.</p>
</aside>

[MATLAB][5] is a computing environment,  pervasive in engineering.
Its simple language, libraries, documentation, and IDE are hard to beat.
It has extensive plotting capabilities, but does not yet support exporting to [PGF/TikZ][7], the toolkit *du jour* for doing vector graphics in LaTeX.
Luckily, there's an excellent library called [matlab2tikz][8] that does the job.

## The workflow

### Structure

Here's the typical structure of a project:

```
diary
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
    └── part_1.m
tex
├── plots
│   └── part_1_mag_db.tex
└── project.tex
```

The `m/` directory (and its subdirectories) are added to MATLAB's path.

### Helpers

The `save_tikz` function is a simple wrapper around `matlab2tikz` that writes to `tex/plots/`, disables auto-updates, and has some other options depending on the project:

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

The `thing` function prints out the contents of a variable, using [`strucdisp`][9] if it's a structure.

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

Don't put all your work into a single script.
In fact, try not to write scripts at all.
This is true for software in general, but *especially* true for MATLAB since everything gets dumped into the persistent workspace.
Functions are the basic self-contained "unit" you should be working with.
Keep them small (say, less than 250 lines).
Don't use workspace (i.e. global) variables to pass along information, don't blindly `load` data, and try to section off any I/O.

Why?
Long scripts/functions are hard to debug and work with.
Relying on global variables makes dependency tracking (what variables must I provide in order to use script/function?) difficult.
Calling `load datafile` instead of `load datafile x y z` is less readable and risks polluting your namespace.
And I/O operations (e.g. plotting) are costly, fragile, and often unneeded.

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

On the first line the outputs explicitly specified.
You could also specify inputs (and provide defaults the usual way, by checking `nargin`).

On the second line I set a code fold using `%%`.
I can run `part_1` *as a script* and have the variables dumped to the workspace using `C-Enter` (making sure to run `clear all` afterwards).
If I want to use the debugger, I can set a few breakpoints and run it *as a function* by hitting `F5`.
Best of both worlds.

On the third line I check whether `part_1` is the "main" thing being run, i.e. that it's not being called from another script or function, in which case I probably want to look at graphs and such.

### Using LaTeX

Here's the basic preamble for working with PGF/TikZ.
<aside style="margin-bottom: -1em">
<p>Don't worry about the <code>pgfplotsset</code> syntax. I don't understand it, either.</p>
</aside>

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

And to include a plot:

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

The relatively new `external` library renders plots to separate numbered PDF files, e.g. `project_figure-0.pdf`.
Plots are re-rendered when their source file is modified.
There are a few annoying details you should be aware of:

* When you remove or reorder plots, the numbering gets messed up and you'll have to delete all the previous renderings.
* You have to configure your editor to use `-shell-escape` (see below)

I'm using `lualatex`, which is included with TeX Live.
The original `pdflatex` runs out of memory (uses a fixed amount, difficult to configure) easily when rendering TikZ plots.
I haven't run into any compatability issues yet.

As a rule of thumb, you should try to limit your generated TikZ plots to less than 250kb, to keep render time short.
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
ix = resample_for_tikz(x, 2e3);

figure;
plot(f(ix)/1e3, db(esd(ix)));
xlabel('Frequency (kHz)');
ylabel('Magnitude (dB)');
save_tikz('Energy spectral density of $x(t)$');
{% endhighlight %}

### Configuring your editor

In Emacs (with AUCtex), add the following to your `init.el`:

{% highlight lisp %}
(setq TeX-engine 'lualatex)
{% endhighlight %}

In Vim, stick the following in your `.vimrc`.
I got fed up with Vim-LaTeX, so this just sets the local `:make` command:

{% highlight vim %}
au FileType * setlocal mp=latexmk\ -pdf\ -e\ '$pdflatex=\"lualatex\ -file-line-error\ -shell-escape\ -synctex=1\ \\%S\ \\%O\"'\ -f\ '%'
{% endhighlight %}

## Conclusion

I think that about covers everything.
I'm really happy with this workflow, I'm certain that it improved the quality of my work as well as my productivity, and I hope that the information here is useful to others.

[0]: http://wikipedia.org/wiki/LaTeX
[1]: http://nitens.org/taraborelli/latex
[2]: http://tex.stackexchange.com/questions/110133/visual-comparison-between-latex-and-word-output-hyphenation-typesetting-ligat
[3]: http://www.tug.org/texlive
[4]: http://wikibooks.org/wiki/LaTeX
[5]: http://mathworks.com
[7]: http://wikipedia.org/wiki/PGF/TikZ
[8]: https://github.com/nschloe/matlab2tikz
[9]: http://www.mathworks.com/matlabcentral/fileexchange/13500-structure-outline/content/strucdisp.m
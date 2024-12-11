# RLumUnmix

The **R** package `'RLumUnmix'` is a collection of R functions intended to be used for provenance analyses. It allows you to generate synthetic TL and OSL signals based on the parameters of the physical model ["Bailey2001"](https://www.sciencedirect.com/science/article/pii/S1350448700001001), using the R packages [Luminescence](https://github.com/R-Lum/Luminescence) and [RLumModel](https://github.com/R-Lum/RLumModel). Different parameters enable to generate various signals corresponding to different sources, that can then be "mixed" synthetically with the [sandbox](https://github.com/coffeemuggler/sandbox) package. Finally, the extracted synthetic sediment section can be unmixed using the [fingerPro](https://github.com/eead-csic-eesa/fingerPro) package (included in `'RLumUnmix'`) in order to decompose the set of luminescence signals into provenance components (percentage of contribution of each source). Additionally, this package can also be used to perform the same analyses but from "real world" sediments, by reading data from BIN/BINX files.

Two results of the unmixing of two sources:

<img src="https://github.com/user-attachments/assets/d98bd5be-43ac-48a2-852a-2782e5d29a84" alt="results_image" width="450"/>
<img src="https://github.com/user-attachments/assets/e495f11c-0525-4d52-b85b-c8dbf54c8481" alt="b0f7647f-d192-4fb2-9ee1-fe618e904bb4![Uploading b0f7647f-d192-4fb2-9ee1-fe618e904bb4.png…]()" width="450"/>




## Installation

To install `'RLumUnmix', run

``` r
devtools::install_github("zazallegri/RLumUnmix")
```

## Using RLumUnmix

Examples of how to use the package are available in folder **/example_code**. You can learn how to use basic functions of the package, create and unmix synthetic sediments stemming from two/three sources as well as unmixing sediments from actual samples. These scripts show the workflow to perform those operations, as well as the format of the data needed at every step. Most of the understanding of this package will come through those example scripts.

Most functions are made of sequences of functions from `'RLumModel'`, `'Luminescence'`, `'sandbox'` and `'fingerPro'` packages. Please refer to their respective documentation.

## Contribute

This package provides a basis for sediment provenance analysis and will hopefully be used and enhanced by some members of the community. Don't hesitate to fork the package, modify/implement new functions, and make pull requests! 

## Note 

The package comes without any guarantee!

It package was created by Balthazar Allegri (balthazar.allegri@bluewin.ch) over the course of 4 months, in the context of my civil service, as part of the [ICE group](https://wp.unil.ch/ice/) of the University of Lausanne (UNIL). I was supervised by Christoph Schmidt (christoph.schmidt@unil.ch), Senior Scientist at UNIL. 

Note that I had no prior knowledge on luminescence or R-programming, comments/improvements are hence most welcome!





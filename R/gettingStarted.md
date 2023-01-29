# Welcome to EPoS-MoL

While the official shorthand nomenclature of lipids is a first and important step towards a reporting quality tool, an additional point score reflects the quality of reported data at an even more detailed granularity. Thus, the **E**mpirical **Po**int **S**core **Mo**del for MS-based **L**ipidomics (EPoS-MoL) is a lipidomics scoring scheme which takes into account all the different layers of analytical information to be obtained by mass spectrometry, chromatography and ion mobility spectrometry and awards scoring points for each of them.

Furthermore the scoring scheme is integrated with the annotation levels as proposed by the official shorthand nomenclature, with a point score which roughly correlates with the annotated compound details. The merit of such a scoring system is the fact, that it abstracts data quality into a number which gives even the non-lipidomics expert an idea about the reporting quality at first glance. Additionally it could serve as an tool supporting self-control of researchers and for data quality assessment in the peer review process.

If you use EPoS-MoL, please cite the following publication:

** TODO Publication **

More information and support for EPoS-ML is available at https://github.com/lifs-tools/eposmol

## EPoS-MoL Webapplication

This web application provides a sample spreadsheet file containing two examples (long and wide format) that should cover the most common output formats of lipid identification tools, or at least a good approximation thereof. The intention is to have a format that is easy to create and process. The wide format may be easier for manual editing, while the long format may be more suitable for a compact representation and for automated creation by tools.

## Data Upload

EPoS-MoL supports upload of identification data in one of the two formats contained in the sample spreadsheet. 

1. Select the 'Upload' tab.
2. Click on 'Browse' to select a spreadsheet file from your local computer.
3. After selection, the file will be inspected for available data sheets.
4. Select the sheet containing your data from the 'Select the sheet to load' drop-down menu.
5. Pick the right table format from the 'Select the table format' drop-down menu.
6. Click on 'Load table' to proceed to load your data.

If you want to clear all data, click on 'Reset'.
Loading a new table will also clear / overwrite any previous data.

## Manual Input

It is also possible to enter lipid identifications manually.

1. Select the 'Manual Input' tab.
2. Enter the lipid name.
3. Select the (EPoS-MoL) lipid category or class.
4. Select the ionization mode.
5. Select the primary classification (MS level).
6. Select the secondary classification (Evidence method).
7. Select the Evidence stream.
8. Click on 'Add score'.

If you have previously loaded a table via the 'Upload' tab, adding items manually will add them to the existing table.

If you want to clear all data, click on 'Reset'.

## Result Views

The user interface displays score results in different tabs:

1. 'Getting Started' holds the user manual.
2. 'Total Scores' holds the final total scoring table, summarized over the entries in 'Individual Scores', for each unique combination of Name and LipidCategoryOrClass (the EPoS-MoL ones). The table will display the TotalScore and the combination of individual Score IDs.
3. 'Individual Scores' holds the intermediate scoring table on the level of individual score contributions, for each unique combination of Name, LipidCategoryOrClass, IonMode and feature contribution to the score and Score ID. Duplicate entries will be removed automatically, so that only one score contribution of the same ID can contribute to one total lipid score. If you add scores manually, they will be added to this table.
4. 'Original Table' holds the table as loaded from file.
5. 'Reference Score Table' holds the score contributions for each scoring feature. Please note that scoring feature IDs do not reflect the lipid category or class, but only the overall feature evidence level.

## Exporting Data

The 'Total Scores' tab allows you to perform two actions:

1. (Optional) Check the lipid names with Goslin<sup>1</sup> for compliance with the latest lipid shorthand nomenclature<sup>2</sup>.
2. Export and download the total score table as a spreadsheet file.

If you choose to check the lipid names, the Goslin library will attempt to parse your lipid names. The resulting columns will be joined to the total scores table, reporting LIPID MAPS Category and Main Class, along with the normalized lipid shorthand name. If parsing fails, 'Unrecognised shorthand name' in the Message column will indicate it.

## Input Data Format - Mandatory column names

The following sections describe the (case-sensitive) column names required to be included for the long and wide format, respectively. Please note that the given order is **NOT** mandatory but recommended.

You can alternatively download the example EXCEL file, which contains separate sheets in the wide and long format.

### Long Format

- Name
- LipidCategoryOrClass
- IonMode
- Feature
- Value

### Wide Format

- Name
- LipidCategoryOrClass
- IonMode
- Nominal mass
- Mass 5ppm
- Mass 1ppm
- Fragment mass 5ppm
- Chromatography (RT)
- Ion mobility (CCS)
- Headgroup 1
- Headgroup 2
- Headgroup 3
- Headgroup 4
- Fatty acyl 1-1
- Fatty acyl 1-2
- Fatty acyl 1-3
- Fatty acyl 1-4
- Fatty acyl 2-1
- Fatty acyl 2-2
- Fatty acyl 2-3
- Fatty acyl 2-4
- Fatty acyl 3-1
- Fatty acyl 3-2
- Fatty acyl 3-3
- Fatty acyl 3-4
- Fatty acyl ratio (sn-position)
- Double bond/ring position
- Functional group ID
- Functional group position
- Phosphate group position
- Double bond configuration
- Stereochemistry

## References

[1] D. Kopczynski, N. Hoffmann, B. Peng, G. Liebisch, F. Spener, and R. Ahrends, "Goslin 2.0 Implements the Recent Lipid Shorthand Nomenclature for MS-Derived Lipid Structures," Anal. Chem., vol. 94, no. 16, pp. 6097–6101, Apr. 2022, doi: https://doi.org/10.1021/acs.analchem.1c05430

[2] G. Liebisch *et al.*, "Update on LIPID MAPS classification, nomenclature, and shorthand notation for MS-derived lipid structures," J Lipid Res, vol. 61, no. 12, pp. 1539–1555, Dec. 2020, doi: https://doi.org/10.1194/jlr.S120001025

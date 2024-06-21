# Using EPoS-MoL

## Data Upload

EPoS-MoL supports upload of identification data in one of the two formats contained in the sample spreadsheet. 

1. Select the 'Upload' tab.
2. Click on 'Browse' to select a spreadsheet file from your local computer or click on 'Load example data' to load the provided example data.
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

The result spreadsheet will contain two sheets:

1. Total Scores and
2. Individual Scores

*Total Scores* will contain the columns *Name*,	*LipidCategoryOrClass*,	*TotalScore*, and *ScoreCode*. It will contain one row per unique lipid name and LipidCategoryOrClass entry.
The *TotalScore* column will contain the sum of the individual point score contributions, whose identities for positive and negative ion mode are provided in the *ScoreCode* column.

If you choose to parse and convert the lipid names, the Goslin library will attempt to parse your lipid names. The resulting columns will be joined to the total scores table, reporting LIPID MAPS Category (*Lipid.Maps.Category*) and Main Class (*Lipid.Maps.Main.Class*), along with the normalized lipid shorthand name (*Normalized.Name*). If parsing fails, 'Unrecognised shorthand name' in the *Message* column will indicate it.

The *Individual Scores* table contains each score contribution per reported feature. So in effect, there will usually be multiple rows for each lipid name.
The table will consist of the following columns: 

- Name (the lipid name or an arbitrary identifier)
- LipidCategoryOrClass (the EPoS-MoL lipid (super-)category or class)
- IonMode ('+' or '-')
- Feature (the feature name)
- Value (a user-definable value)
- Primary (the primary feature dimension)
- Secondary (the secondary fature dimension)
- Fragment (the fragment abbreviation, if applicable)
- ID (a unique ID for the feature coordinate)
- Score (the point score of the feature)

Additionally, if you run Goslin the output may contain a column *Shorthand.Level.CvTerm* that will contain the controlled vocabulary term for the shorthand lipid name level as returned by Goslin. See [PSI-MS](https://www.ebi.ac.uk/ols4/ontologies/ms) for more details.

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

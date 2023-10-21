# MiSeVis
Overview

This pipeline not only outputs the contents of an inputted sequence and structure file but also allows the user to visualize 3D models of a PDB file. The user may also manipulate the model’s parameters and analyze individual amino acids through the generated model. The user may also look for drug matches in the DrugBank database based on the selection of the PDB visualization file.
Retrieve Sequence File

On the Retrieve Sequence Tab there are two options for file input: a Sequence file and a Structure file. The Sequence File tab has a button that is accessed with the “Click to Show Upload Option” button which allows the user to input a file from the computer’s local files. However, the file upload must be in the .FASTA format in order to properly show the syntax of the inputted FASTA file. You may click on the Clear Sequence button to clear the sequence shown or the Clear PDB button to get rid of the output of the Structure File tab. The Structure File tab works very similarly to the Sequence File tab with the exception of the inputted file type. The user-uploaded file must be a Protein Data Bank file instead of a FASTA file.
Visualize PDB File

The Visualize PDB tab allows the user to examine and modify the 3D model of the inputted gene. First, the user must select a gene in the “Choose a Gene to Visualize” dropdown menu. Then, they must specify whether or not they would like to see the normal or mutated structure of the gene. The 3D visualization will appear automatically and the user may use their cursor to examine the different sides of the model. Furthermore, since the model is based on the specified quaternary structure of the PDB file, the user may also hover over individual parts of the protein and see specific amino acids at the precise location on the amino acid sequence. The user may also highlight certain sections of the protein by pressing the “Manipulate the 3D Visualization” button. The first parameter is the amino acid selection, which allows the user to specify the range of amino acids they want highlighted. The second parameter is the representation type for the highlighted amino acids. The third parameter specifies the color of the highlighted regions in the protein. The “Add” button will add the specified parameters while the “Remove” button will remove them. Finally, the button “Click to Show Protein Surface” allows the user to visualize the surface of the entire 3D model. However, the user must have the type parameter set to “surface” in order for the button to work.
Check DrugBank

The Check DrugBank tab is used to identify drug targets for specific genes based on the DrugBank database. The user simply needs to type in the gene name and click the “Output Drug Targets” button. The main panel will show the drug IDs that are linked to the queried gene.
Survival analysis

The Survival analysis tab is used to assess the effect of specific gene expression on survival rates. The user simply needs to select a gene name. The main panel will show the plot.

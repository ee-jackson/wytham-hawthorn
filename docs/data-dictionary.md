## hawthorn-mapping
This folder is located at `data/raw/hawthorn-mapping` and contains a `.csv` file for each focal tree with mapping data for all conspecifics in a 50m^2 quadrat around the focal tree. The `.csv` files are named after their respective focal tree id.

| variable | description |
|:---------|:------------|
| **Tree variables** | |
| `focal_tree` | The focal tree around which the 50m^2 plot was mapped |
| `name` | The id of the tree. There may be multiple rows for each `name`.  |
| `longitude` | longitude in decimal degrees |
| `latitude` | latitude in decimal degrees |
| `solution_status` | Either `fix` (accuracy is within 10cm) or `float` (accuracy is within 50cm) |
| `cs_name` | Co-ordinate system |
| `form` | Either `m` (the individual was made up of multiple stems from the ground), `s` (the individual was comprised of a single main stem from the ground) or  `b` (the individual did not have a stem greater than 3cm circumference at breast height) |
| `reproductive` | Either `y` (the individual had flowers or fruits at the time of recording) or `n` (the individual did not have flowers or fruits at the time of recording) |
| `d` | Either `y` (the tree was dead) or `n` (the tree was not dead) |
| `q` | Either `y` (the tree was broken above 1.3m) or `n` (the tree was not broken above 1.3m) |
| `h` | Either `y` (the tree was leaning or horizontal) or `n` (the tree was not leaning or horizontal) |
| `susie` | Either `y` (the tree had pink tags) or `n` (the tree did not have pink tags) |
| **Stem variables** | |
| `stem` | For individuals whose stems split below 1.3 meters, the stem is either `main` (`DBH` taken below the split) or `secondary` (`DBH` taken above the split at 1.3m) |
| `dbh` | Diameter of the stem at breast height (1.3m) |
| `split_height` | Recorded where `form` = `s` and `stem` = `main`, the height at which the main stem splits below 1.3m |
| `other_d` | Recorded where `form` = `s` and `stem` = `main`, diameter of stem at the specified split height |
|:---------|:------------|
| `notes` | Any notes taken during data collection |

## fruit_set_flowers.csv
Count of flowers on focal trees in 2022

| variable | description |
|:---------|:------------|
| `tree_id` | The focal tree id number |
| `date` | The date the data was collected |
| `phenology_whole_tr` |  A note on the phenology of the tree, either `no flowers`, `majority flowers closed` or `majority flowers open` |
| `branch_id` | The branch id |
| `n_flowers` | A count of flowers on the branch (including damaged flowers) | 
| `n_flowers_damaged` | A count of damaged flowers (i.e. herbivore damage) |
| `bagged` | Did the branch have a pollinator exclusion mesh over it at the time of data collection? |
| `branch_length_cm` | Only recorded once for each branch - the length of the branch in cm from it's tip to the ID tag |
| `notes` | Any notes taken during data collection |

## fruit_set_initial.csv 
Count of green fruits on focal trees in 2022

| variable | description |
|:---------|:------------|
| `tree_id` | The focal tree id number |
| `date` | The date the data was collected |
| `branch_id` | The branch id |
| `n_fruits` | A count of fruits on the branch | 
| `bagged` | Did the branch have a pollinator exclusion mesh over it at the time of data collection? |
| `notes` | Any notes taken during data collection |

## fruit_set_final.csv
Count of red fruits on focal trees in 2022

| variable | description |
|:---------|:------------|
| `tree_id` | The focal tree id number |
| `date` | The date the data was collected |
| `branch_id` | The branch id |
| `n_fruits` | A count of fruits on the branch | 
| `notes` | Any notes taken during data collection |

## fruit_set_2023.csv
Flower and immature fruit counts in 2023.
Flowers were counted on 31st May 2023 and immature fruits were counted on 26th and 29th June 2023.

| variable | description |
|:---------|:------------|
| `tree_id` | The focal tree id number |
| `branch_id` | The branch id |
| `n_immature_fruits` | A count of immature fruits on the branch | 
| `n_flowers` | A count of flowers on the branch (not including damaged or galled flowers) | 
| `n_damaged_flowers` | A count of damaged flowers (i.e. herbivore damage) |
| `n_galled_flowers` | A count of galled flowers (closed flowers with apparent herbivore emergence/entrance hole) |
| `notes` | Any notes taken during data collection |

## fruit_drop_data.csv
Monthly fruit counts between 2021-08-05 and 2022-03-07

| variable | description |
|:---------|:------------|
| `date` | The date the data was collected |
| `tree_id` | The focal tree id number |
| `branch_id` | The branch id |
| `n_fruit` | A count of fruits on the branch | 
| `length_cm` | Only recorded once for each branch - the length of the branch in cm from it's tip to the ID tag |
| `notes` | Any notes taken during data collection |

## flower_and_fruit_counts_hawthorn_2023.xlsx
Fruit and flower counts in 2023. This is an excel workbook where sheet 1 contains the data and sheet 2 contains metadata.

| variable | description |
|:---------|:------------|
| `tree_id` | The focal tree id number |
| `branch_id` | The branch id |
| `n_flowers` | A count of flowers on the branch (not including damaged or galled flowers) | 
| `n_damaged_flowers` | A count of damaged flowers (i.e. herbivore damage) |
| `n_galled_flowers` | A count of galled flowers (closed flowers with apparent herbivore emergence/entrance hole) |
| `n_immature_fruits` | A count of immature fruits on the branch | 
| `n_mature_fruits` | A count of mature fruits on the branch | 
| `notes` | Any notes taken during data collection |

## hawthorn-herbivory.csv

| variable | description |
|:---------|:------------|
| `tree_id` | The focal tree id number |
| `sample` | The sample id  |
| `date` | The date the herbivory was recorded |
| `n_leaves_assessed` | The number of leaves assessed for herbivory. This is usually 50, but less for individuals which had less than 50 leaves. |
| `leaves_w_herbivory` | The number of leaves assessed which showed evidence of herbivory |
| `little_green_folds_total` | Count of little green folds across all `leaves_w_herbivory` |
| `upper_mines_total` | Count of upper mines across all `leaves_w_herbivory` |
| `tents_total` | Count of tents across all `leaves_w_herbivory` |
| `speckly_webs_total` | Count of speckly webs across all `leaves_w_herbivory` |
| `dark_lower_mines_total` | Count of dark lower mines across all `leaves_w_herbivory` |
| `pale_lower_mines_total` | Count of pale lower mines across all `leaves_w_herbivory`|
| `lice_total` | Count of leaves which had plant lice |
| `rust_presence` | `TRUE` (presence) or `FALSE` (absence) of rust on leaves. Leaves with rust were not included in the count of `leaves_w_herbivory`.  |
| `brown_curls_total` | Count of brown curls across all `leaves_w_herbivory`. The brown curls were originally classified as tents and counted in `tents_total`. Where `brown_curls_total` = `NA`, brown curls are included in `tents_total`. |

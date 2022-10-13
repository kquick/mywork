# Revision history for mywork

## 1.0.1.0 -- 2022-10-13

### Operational

* NOTE: using this version will update the stored project information in a manner
  that is incompatible with version 1.0.0.0.  If you intend to return to version
  1.0.0.0, backup your project data file before saving from this version.

* Adds ability to specially recognize keywords and dates in note titles and
  sort and highlight accordingly.

* Cannot edit or delete a note unless it was created within mywork.  Project
  location notes and auto-generated notes such as repository location relations
  cannot be edited.

* Ctrl-S immediate save not available when displaying any modal other than the
  FileMgr modal.

* Enter in project search box clears search box but retains current selection.

* Miscellaneous small behavioral bug fixes.

### Internal/Implementation

* Improved internal efficiency when reading local location information.

* No storage of transient data in projects storage file.

* Split into a minimal application utilizing a library

## 1.0.0.0 -- 2022-10-01

* First version.

module Web.API
  (

  ) where

{--
  API Datatypes, these match 1:1 with the tables
  in the database.

  Models:
    Employee

    Project

    Department

    Manager
--}

data Employee = Employee
  { empID  :: Text -- UUID
  , fname  :: Text
  , lname  :: Text
  , deptID :: Integer
  }

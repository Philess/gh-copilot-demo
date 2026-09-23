using Xunit;

// The Album model store is a process-wide static list (by design, to keep
// create/update/delete operations without a database for this demo API).
// Running test classes in parallel can interleave create/delete calls and
// cause one test to observe another test's data via id reuse, so
// parallelization across test collections is disabled for this assembly.
[assembly: CollectionBehavior(DisableTestParallelization = true)]
